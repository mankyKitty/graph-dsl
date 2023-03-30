window.onload = function() {

    // Should be true if there's an automated demo running.
    let demo = false;

    // Buttons.
    let backButton = document.getElementById('backButton');
    let filterButton = document.getElementById('filterButton');
    let autoButton = document.getElementById('autoButton');

    // The current network.
    let network = null;
    let currentCursor = null;
    let currentCursorId = null;
    let metadata = null;

    // GET request for retrieving JSON.
    // https://stackoverflow.com/questions/12460378/how-to-get-json-from-url-in-javascript
    let getJSON = function(url, callback) {
        let xhr = new XMLHttpRequest();
        xhr.open('GET', url, true);
        xhr.responseType = 'json';
        //xhr.setRequestHeader('Access-Control-Allow-Origin', '*');
        xhr.onload = function() {
            let status = xhr.status;
            if (status === 200) {
                callback(null, xhr.response);
            } else {
                callback(status, xhr.response);
            }
        };
        xhr.send();
    };

    // POST request for retrieving JSON.
    let postJSON = function(url, callback, data) {
        let xhr = new XMLHttpRequest();
        xhr.open('POST', url, true);
        xhr.responseType = 'json';
        xhr.setRequestHeader('Content-Type', 'application/x-www-form-urlencoded');
        //xhr.setRequestHeader('Content-Length', data.length);
        //xhr.setRequestHeader('Access-Control-Allow-Origin', 'http://localhost:8080');
        xhr.onload = function() {
            let status = xhr.status;
            if (status === 200) {
                callback(null, xhr.response);
            } else {
                callback(status, xhr.response);
            }
        };
        xhr.send(JSON.stringify(data));
    };

    // Function for handling the result of a move/back request.
    let moveRequestHandler = function(err, data) {
        if (err === 404) {
            alert('Could not perform a move action:\nThe server did not return any data for the requested action.');
        } else if (err === 400) {
            alert('Could not perform a move action:\nThe server encountered an error while trying to complete the requested action.');
        } else if (err !== null) {
            alert('Could not perform a move action:\nHTTP status code was ' + err);
        } else {
            getJSON('http://localhost:8080/getGraph', graphRequestHandler);
        }
    };

    // Function for getting synonyms for a TF.
    let getSynonyms = function(name, data) {
        let row = data.find((item) => item.Name.toLowerCase() === name.toLowerCase());
        if (row !== undefined) {
            return row.Synonyms;
        } else {
            return [""];
        }
    }

    // Function for filtering visible nodes.
    // Defaults to no filter which shows all nodes in the network.
    let applyFilter = function(filterText = "") {
        let edgeUpdates = [];
        let nodeUpdates = [];

        // If the filter is blank, make everything visible.
        if (filterText.length < 1) {
            for (edgeId in network.body.edges) {
                let edge = network.body.edges[edgeId];
                if (edge.toId !== edge.fromId) {
                    edgeUpdates.push({ id: edgeId, hidden: false });
                }
            }
            for (nodeId in network.body.nodes) {
                nodeUpdates.push({ id: parseInt(nodeId), hidden: false });
            }
        // Otherwise, find the edges and nodes that should be hidden.
        } else {
            for (edgeId in network.body.edges) {
                let edge = network.body.edges[edgeId];
                // Ignore this edge if both ends are the same. This should
                // only be edges that start and end on the cursor.
                if (edge.toId !== edge.fromId) {
                    // Get the node that's not the cursor.
                    let nodeId = edge.fromId === currentCursorId ? edge.toId : edge.fromId;
                    let node = network.body.nodes[nodeId];

                    // Determine if this node (and its edge) should be
                    // visible or not.
                    if (node.options.title.includes(filterText)) {
                        edgeUpdates.push({ id: edgeId, hidden: false });
                        nodeUpdates.push({ id: nodeId, hidden: false });
                    } else {
                        edgeUpdates.push({ id: edgeId, hidden: true });
                        nodeUpdates.push({ id: nodeId, hidden: true });
                    }
                }
            }
        }

        // Update the network.
        network.body.data.edges.update(edgeUpdates);
        network.body.data.nodes.update(nodeUpdates);
    }

    // Function for generating a graph with supplied JSON data.
    let generateGraph = function(sourceData) {
        // Clear the current network if it exists already.
        if (network !== null) {
            network.destroy();
            network = null;
            currentCursor = null;
            currentCursorId = null;
        }

        // Create an array for the node data to use later.
        currentCursorId = sourceData.Vertex.Value;
        currentCursor = sourceData.Vertex.Tag;

        // The button for the Crp traversal example should only work if
        // we're actually on Crp.
        if (currentCursor === "Crp") {
            document.getElementById('autoButton').removeAttribute("disabled");
        } else {
            document.getElementById('autoButton').setAttribute("disabled", "disabled");
        }

        let nodeData = [{id: currentCursorId, label: sourceData.Vertex.Tag}];
        for (edge of sourceData.Edges) {
            if (nodeData.find((item) => item.id === edge.End.Value) === undefined) {
                nodeData.push({id: edge.End.Value, label: edge.End.Tag});
            }
            if (nodeData.find((item) => item.id === edge.Start.Value) === undefined) {
                nodeData.push({id: edge.Start.Value, label: edge.Start.Tag});
            }
        }

        // Create an array for the edge data to use later.
        let edgeData = [];
        for (edge of sourceData.Edges) {
            let currentEdge = {from: edge.Start.Value, to: edge.End.Value}
            if (edgeData.find((item) => item.from === currentEdge.from && item.to === currentEdge.to) === undefined) {
                if (currentEdge.to === sourceData.Vertex.Value) {
                    currentEdge.color = 'red'
                } else {
                    currentEdge.color = 'green'
                }
                edgeData.push(currentEdge);
            }
        }

        // Set up the proper node array for visualisation.
        let nodes = new vis.DataSet(nodeData);

        // Set up the proper edge array for visualisation.
        let edges = new vis.DataSet(edgeData);

        // Select the div element to put the network in.
        let container = document.getElementById('networkDiv');

        // Provide the data in the vis format.
        let data = {
            nodes: nodes,
            edges: edges
        };

        // Options for creating the network.
        let options = {
            edges: {
                arrows: 'to',
                smooth: false
            },
            interaction: {
                dragNodes: false,
                navigationButtons: true
            },
            layout: {
                randomSeed: 9001
            },
            nodes: {
                shape: 'ellipse',
                fixed: true
            },
            physics: {
                enabled: false
            }
        };

        // Initialize your network!
        network = new vis.Network(container, data, options);
        //network.stabilize(60);

        // Set node positions.
        // Selected node should be in the centre.
        network.moveNode(currentCursorId, 0, 0);

        // Calculate the angle between each target node.
        const angle = (2 * Math.PI) / (nodeData.length - 1)

        // Fixed radius for the target nodes.
        const radius = 350;

        // For every other node, position it in a circle with the given
        // radius around the central node.
        let counter = 0;
        for (row of nodeData) {
            if (row.id !== currentCursorId) {
                network.moveNode(row.id, radius * Math.cos(counter * angle), radius * Math.sin(counter * angle));
                counter++;
            }
        }

        // Function for handling the result of a metadata request.
        let metadataRequestHandler = function(err, data) {
            if (err === 404) {
                console.error('Could not retrieve metadata:\nThe server did not return any metadata.');
            } else if (err === 400) {
                console.error('Could not retrieve metadata:\nThe server encountered an error while trying to send metadata.');
            } else if (err !== null) {
                console.error('Could not retrieve metadata:\nHTTP status code was ' + err);
            } else {
                for (row of nodeData) {
                    let node = network.body.nodes[row.id];
                    node.setOptions({title: getSynonyms(row.label, data).join("\n")});
                }
                metadata = data;
            }
        };

        // Try to add metadata to the nodes.
        if (metadata === null) {
            getJSON('http://localhost:8080/getMetadata', metadataRequestHandler);
        } else {
            for (row of nodeData) {
                let node = network.body.nodes[row.id];
                node.setOptions({title: getSynonyms(row.label, metadata).join("\n")});
            }
        }

        // Apply filter if the user has entered a value and this isn't
        // a demo.
        if (demo === false) {
            let filterInput = document.getElementById('filterInput');
            let filterText = filterInput.value;
            applyFilter(filterText);
        }

        // Clicking on a network node.
        network.on('click', function(properties) {
            if (demo === true) return;
            let ids = properties.nodes;
            let clickedNodes = nodes.get(ids);
            if (clickedNodes.length > 0) {
                network.unselectAll();
                postJSON('http://localhost:8080/move', moveRequestHandler, {'moveOp':'ToVertex','moveInputs':{'Tag':clickedNodes[0].label,'Value':clickedNodes[0].id}});
            }
        });
    }

    // Function for handling the result of a graph request.
    let graphRequestHandler = function(err, data) {
        if (err === 404) {
            console.error('Could not retrieve current graph state:\nThe server did not return any data.');
        } else if (err === 400) {
            console.error('Could not retrieve current graph state:\nThe server encountered an error while trying to send data.');
        } else if (err !== null) {
            console.error('Could not retrieve current graph state:\nHTTP status code was ' + err);
        } else {
            generateGraph(data);
        }
    };

    // Call server for the starting network JSON.
    getJSON('http://localhost:8080/getGraph', graphRequestHandler);

    // Clicking on the back button.
    backButton.addEventListener('click', function() {
        if (demo === true) return;
        postJSON('http://localhost:8080/move', moveRequestHandler, {'moveOp':'Back','moveInputs':[]});
    });

    // Clicking on the filter button.
    // It should show only those targets that have the text in their
    // synonyms.
    filterButton.addEventListener('click', function() {
        if (demo === true) return;
        let filterInput = document.getElementById('filterInput');
        let filterText = filterInput.value;
        applyFilter(filterText);
    });

    // Button for demonstrating a traversal with Crp as the starting
    // point.
    autoButton.addEventListener('click', function () {
        let autoText = document.getElementById('autoText');

        // Don't do anything if a demo is already running (it shouldn't
        // happen anyway, though).
        if (demo === true) return;

        // Interval between moves.
        const interval = 3000;

        // Turns the buttons back on when we're done.
        const resetButtons = function () {
            backButton.removeAttribute("disabled");
            filterButton.removeAttribute("disabled");

            // Don't turn the auto button back on if we had an error
            // and aren't at Crp.
            if (currentCursor === "Crp") autoButton.removeAttribute("disabled");

            // Hide any status text.
            autoText.innerHTML = "";
            autoText.setAttribute("style", "display: none");
        };

        // Function for moving forward.
        const forwardMove = function (id, label) {
            postJSON('http://localhost:8080/move', function (err, data) {
                if (err) {
                    alert('Failed to move graph zipper.');
                    demo = false;
                    resetButtons();
                } else {
                    getJSON('http://localhost:8080/getGraph', graphRequestHandler);
                }
            }, {'moveOp':'ToVertex','moveInputs':{'Tag':label,'Value':id}});
        };

        // Function for moving backward.
        const backwardMove = function () {
            postJSON('http://localhost:8080/move', function (err, data) {
                if (err) {
                    alert('Failed to move graph zipper.');
                    demo = false;
                    resetButtons();
                } else {
                    getJSON('http://localhost:8080/getGraph', graphRequestHandler);
                }
            }, {'moveOp':'Back','moveInputs':[]});
        };

        // If we're not already running a demo...
        if (demo !== true) {

            // Disable the buttons and set that we're running a demo.
            backButton.setAttribute("disabled", "disabled");
            filterButton.setAttribute("disabled", "disabled");
            autoButton.setAttribute("disabled", "disabled");
            demo = true;

            // Show status text.
            autoText.innerHTML = `Moving to Fis (right-most node) in ${interval/1000} seconds...`;
            autoText.removeAttribute("style");

            // Clear any current filter.
            applyFilter();

            try {

                // Move to Fis.
                setTimeout(function () {
                    if (demo === false) return;
                    forwardMove(0, 'Fis');
                    autoText.innerHTML = `Moving to NtrC (top-top-right node) in ${interval/1000} seconds...`;
                }, interval*1);

                // Move to NtrC.
                setTimeout(function () {
                    if (demo === false) return;
                    forwardMove(13, 'NtrC');
                    autoText.innerHTML = `Moving to Cbl (left-most node) in ${interval/1000} seconds...`;
                }, interval*2);

                // Move to Cbl.
                setTimeout(function () {
                    if (demo === false) return;
                    forwardMove(119, 'Cbl');
                    autoText.innerHTML = `Moving back to NtrC (right-most node) in ${interval/1000} seconds...`;
                }, interval*3);

                // Move back to NtrC.
                setTimeout(function () {
                    if (demo === false) return;
                    backwardMove();
                    autoText.innerHTML = `Moving back to Fis (right-most node) in ${interval/1000} seconds...`;
                }, interval*4);

                // Move back to Fis.
                setTimeout(function () {
                    if (demo === false) return;
                    backwardMove();
                    autoText.innerHTML = `Moving back to Crp (right-most node) in ${interval/1000} seconds...`;
                }, interval*5);

                // Move back to Crp.
                setTimeout(function () {
                    if (demo === false) return;
                    backwardMove();
                    demo = false;
                    resetButtons();
                }, interval*6);

            // Reset things if there's any errors caught.
            } catch (error) {
                demo = false;
                resetButtons();
                alert(`Could not complete traversal example:\n${error.message}.`);
            }
        }
    });
};