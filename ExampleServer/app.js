const express = require('express');
const nunjucks = require('nunjucks');
const http = require('http');

const app = express();
const port = 3000;

nunjucks.configure('views', {
    autoescape: true,
    express: app
});

function getGraph () {
    return new Promise((resolve, reject) => { 
        http.get('http://localhost:8080/getGraph', (response) => {
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
                    const parsedData = JSON.parse(rawData);
                    console.log(parsedData);
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
    getGraph().then((graphData) => {
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
                try {
                    const parsedData = JSON.parse(rawData);
                    console.log(parsedData);
                    res.redirect('/');
                } catch (error) {
                    console.error(`Got error: ${error.message}`); 
                    res.redirect('/');    
                }
            });
        });
        
        req.on('error', (error) => {
            console.error(`Got error: ${error.message}`);  
            res.redirect('/');        
        });

        request.write(data);
        request.end();
    } else {
        console.error(`Did not get correct paramaters for move operation (Got ${tag} and ${value}).`);
        res.redirect('/');
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
            try {
                const parsedData = JSON.parse(rawData);
                console.log(parsedData);
                res.redirect('/');
            } catch (error) {
                console.error(`Got error: ${error.message}`); 
                res.redirect('/');    
            }
        });
    });
    
    req.on('error', (error) => {
        console.error(`Got error: ${error.message}`);  
        res.redirect('/');        
    });

    request.write(data);
    request.end();
});

app.listen(port, () => {
  console.log(`Example app listening on port ${port}`);
});