const express = require('express');
const nunjucks = require('nunjucks');
const http = require('http');

const app = express();
const port = 3000;

nunjucks.configure('views', {
    autoescape: true,
    express: app
});

// https://github.com/expressjs/express/issues/3226#issuecomment-283979763
app.use('/static', express.static(__dirname + '/static'))

function getRequest (operation) {
    return new Promise((resolve, reject) => {
        http.get(`http://localhost:8080/${operation}`, (response) => {
            const { statusCode } = response;

            let error;
            if (statusCode !== 200) {
                error = new Error('Request Failed.\n' +
                    `Status Code: ${statusCode}`);
            }
            if (error) {
                console.error(error.message);
                response.resume();
                reject(error);
            }

            response.setEncoding('utf8');
            let rawData = '';
            response.on('data', (chunk) => { rawData += chunk; });
            response.on('end', () => {
                try {
                    /*const parsedData = JSON.parse(rawData);
                    console.log(parsedData);*/
                    console.log("The operation completed successfully");
                    resolve(rawData);
                } catch (error) {
                    console.error(`Got error: ${error.message}`);
                    reject(error);
                }
            });
        }).on('error', (error) => {
            console.error(`Got error: ${error.message}`);
            reject(error);
        });
    });
}

app.get('/', (req, res) => {
    console.log('Loading /');
    getRequest('getGraph').then((graphData) => {
        res.render('network.njk', { data: graphData });
    })
    .catch((error) => {
        res.render('default.njk', { contents: error.message });
    });
});

app.get('/move', (req, res) => {
    console.log('Loading /move');
    let tag = req.query.label;
    let value = parseInt(req.query.id);
    if (tag !== undefined && value !== undefined ) {
        console.log(`Trying to move with ${tag} and ${value}.`);
        let data = JSON.stringify({ "moveOp": "ToVertex", "moveInputs": { "Tag": tag, "Value": value }});
        let options = {
          hostname: 'localhost',
          port: 8080,
          path: '/move',
          method: 'POST',
          headers: {
               'Content-Type': 'application/x-www-form-urlencoded',
               'Content-Length': data.length
             }
        };
        let request = http.request(options, (response) => {
            response.setEncoding('utf8');
            let rawData = '';
            response.on('data', (chunk) => { rawData += chunk; });
            response.on('end', () => {
                console.log(`Got back status ${response.statusCode}`);
                if (response.statusCode === 404) {
                    console.error('The requested move action was invalid for this graph.');
                    res.status(404);
                    res.send('The requested move action was invalid for this graph.');
                } else {
                    try {
                        /*const parsedData = JSON.parse(rawData);
                        console.log(parsedData);*/
                        //res.redirect('/');
                        console.log("The operation completed successfully");
                        getRequest('getGraph').then((graphData) => {
                            res.send(graphData);
                        })
                        .catch((error) => {
                            console.error(`Got error: ${error.message}`);
                            res.send(`Got error: ${error.message}`);
                        });
                    } catch (error) {
                        console.error(`Got error: ${error.message}`);
                        //res.redirect('/');
                        res.status(400);
                        res.send(`Got error: ${error.message}`);
                    }
                }
            });
        });

        request.on('error', (error) => {
            console.error(`Got error: ${error.message}`);
            //res.redirect('/');
            res.status(400);
            res.send(`Got error: ${error.message}`);
        });

        request.write(data);
        request.end();
    } else {
        console.error(`Did not get correct paramaters for move operation (Got ${tag} and ${value}).`);
        //res.redirect('/');
        res.status(400);
        res.send(`Did not get correct paramaters for move operation (Got ${tag} and ${value}).`);
    }
});

app.get('/back', (req, res) => {
    console.log('Loading /back');
    console.log('Trying to move back.');
    let data = JSON.stringify({ "moveOp": "Back", "moveInputs": []});
    let options = {
      hostname: 'localhost',
      port: 8080,
      path: '/move',
      method: 'POST',
      headers: {
           'Content-Type': 'application/x-www-form-urlencoded',
           'Content-Length': data.length
         }
    };
    let request = http.request(options, (response) => {
        response.setEncoding('utf8');
        let rawData = '';
        response.on('data', (chunk) => { rawData += chunk; });
        response.on('end', () => {
            console.log(`Got back status ${response.statusCode}`);
            if (response.statusCode === 404) {
                console.error('The requested move action was invalid for this graph.');
                res.status(404);
                res.send('The requested move action was invalid for this graph.');
            } else {
                try {
                    /*const parsedData = JSON.parse(rawData);
                    console.log(parsedData);*/
                    //res.redirect('/');
                    console.log("The operation completed successfully");
                    getRequest('getGraph').then((graphData) => {
                        res.send(graphData);
                    })
                    .catch((error) => {
                        console.error(`Got error: ${error.message}`);
                        res.send(`Got error: ${error.message}`);
                    });
                } catch (error) {
                    console.error(`Got error: ${error.message}`);
                    //res.redirect('/');
                    res.status(400);
                    res.send(`Got error: ${error.message}`);
                }
            }
        });
    });

    request.on('error', (error) => {
        console.error(`Got error: ${error.message}`);
        //res.redirect('/');
        res.status(400);
        res.send(`Got error: ${error.message}`);
    });

    request.write(data);
    request.end();
});

app.get('/getMetadata', (req, res) => {
    console.log('Loading /getMetadata');
    getRequest('getMetadata').then((rawData) => {
        /*const parsedData = JSON.parse(rawData);
        console.log(parsedData);*/
        console.log("The operation completed successfully");
        res.send(rawData);
    })
    .catch((error) => {
        console.error(`Got error: ${error.message}`);
        res.send(`Got error: ${error.message}`);
    });
});

app.listen(port, () => {
  console.log(`Example app listening on port ${port}`);
});