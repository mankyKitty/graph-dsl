window.onload = function() {

    // Should be true if there's an automated demo running.
    let demo = false;

    // Buttons.
    let backButton = document.getElementById('backButton');
    let filterButton = document.getElementById('filterButton');
    let autoButton = document.getElementById('autoButton');
    let historyText = document.getElementById('historyText');
    let jumpToSelect = document.getElementById('jumpToSelect');
    let jumpToButton = document.getElementById('jumpToButton');
    let moveByScoreButton = document.getElementById('moveByScoreButton');
    let historyStatus = document.getElementById('historyStatus');
    let forwardButton = document.getElementById('forwardButton');

    // The current network.
    let mainNetwork = null;
    let currentCursor = null;
    let currentCursorId = null;
    let metadata = null;
    let dropdownVertices = null;

    // Overview of the whole graph.
    let overviewNetwork = null;

    // Current user filter.
    let currentFilterText = '';

    // GET request for retrieving JSON.
    // https://stackoverflow.com/questions/12460378/how-to-get-json-from-url-in-javascript
    let getJSON = function(url, callback) {
        let xhr = new XMLHttpRequest();
        xhr.open('GET', url, true);
        xhr.responseType = 'json';
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
    let getSynonyms = function (name, data) {

        // Try to find a row in the metadata that has the supplied name.
        let row = data.find((item) => item.Name.toLowerCase() === name.toLowerCase());

        // Return the list of synonyms if it is found.
        if (row !== undefined) {
            return row.Synonyms;

        // Otherwise return an empty list.
        } else {
            return [""];
        }
    }

    // Function for getting all metadata for a TF and putting it into a string
    // to be displayed in the graph popups.
    let getMetadataString = function (name, data) {

        // Try to find a row in the metadata that has the supplied name.
        let row = data.find((item) => item.Name.toLowerCase() === name.toLowerCase());

        // Return the metadata to display if it is found.
        if (row !== undefined) {
            let lines = [];
            lines.push(`Identifier: ${row.Id}`);
            lines.push(`Name: ${row.Name}`);
            lines.push('Synonyms:');
            for (text of row.Synonyms) {
                lines.push(`- ${text}`);
            }
            if (row.GeneCoding !== undefined) {
                lines.push(`Gene Coding: ${row.GeneCoding}`);
                //lines.push(`Active Conformations: ${row.ActiveConformations}`);
                //lines.push(`Inactive Conformations: ${row.InactiveConformations}`);
                //lines.push(`Active Conformations Synonyms: ${row.ActiveConformationsSynonyms}`);
                //lines.push(`Inactive Conformations Synonyms: ${row.InactiveConformationsSynonyms}`);
                //lines.push(`Active Conformations Effector Names: ${row.ActiveConformationsEffectorName}`);
                //lines.push(`Inactive Conformations Effector Names: ${row.InactiveConformationsEffectorName}`);
                //lines.push(`Active Conformations Effector Synonyms: ${row.ActiveConformationsEffectorSynonyms}`);
                //lines.push(`Inactive Conformations Effector Synonyms: ${row.InactiveConformationsEffectorSynonyms}`);
                lines.push(`Symmetry: ${row.Symmetry}`);
                lines.push(`Family: ${row.Family}`);
                lines.push(`Connectivity Class: ${row.ConnectivityClass}`);
                lines.push(`Sensing Class: ${row.SensingClass}`);
                lines.push('Confirmation Evidence:');
                for (text of row.ConfirmationEvidence) {
                    lines.push(`- ${text}`);
                }
                lines.push('Additive Evidence:');
                for (text of row.AdditiveEvidence) {
                    lines.push(`- ${text}`);
                }
                lines.push(`Confidence Level: ${row.ConfidenceLevel}`);
                lines.push('Comfirmation reference identifiers (PMID):');
                for (text of row.Pmids) {
                    lines.push(`- ${text}`);
                }
            }
            return lines.join('\n');

        // Otherwise return a placeholder string.
        } else {
            return 'No metadata available';
        }
    }

    // Function for filtering visible nodes.
    // Defaults to no filter which shows all nodes in the main network.
    let applyFilter = function(filterText = '') {

        // List of updates to push to edges and nodes.
        // The objects in these arrays will include the edge/node id to change
        // as well as the options that will be changed for that edge/node. In
        // this case, it will be the "hidden" option.
        let edgeUpdates = [];
        let nodeUpdates = [];

        // If the filter is blank, make everything visible.
        if (filterText.length < 1) {

            // Add an update for each edge in the main network.
            for (edgeId in mainNetwork.body.edges) {
                let edge = mainNetwork.body.edges[edgeId];

                // We don't need to worry about any edges that have the same
                // node on both ends since the only one that can have that is
                // the regulator, which is always visible.
                if (edge.toId !== edge.fromId) {
                    edgeUpdates.push({ id: edgeId, hidden: false });
                }
            }

            // Add an update for each node in the main network.
            for (nodeId in mainNetwork.body.nodes) {

                // We need to use "parseInt" here, as the actual ID is an int
                // but the key is returned as a string when we iterate.
                nodeUpdates.push({ id: parseInt(nodeId), hidden: false });
            }

            // Update the button for moving by score.
            moveByScoreButton.innerHTML = 'Move to most connected neighbour'
        // Otherwise, find the edges and nodes that should be hidden.
        } else {

            // Go through all edges in the main network.
            for (edgeId in mainNetwork.body.edges) {
                let edge = mainNetwork.body.edges[edgeId];

                // Ignore this edge if both ends are the same. This should
                // only be edges that start and end on the cursor.
                if (edge.toId !== edge.fromId) {
                    // Get the node that's not the cursor.
                    let nodeId = edge.fromId === currentCursorId ? edge.toId : edge.fromId;
                    let node = mainNetwork.body.nodes[nodeId];

                    // Determine if this node (and its edge) should be
                    // visible or not.
                    if (getSynonyms(node.options.label, metadata).find(synonym => synonym.toLowerCase().includes(filterText.toLowerCase()))) {
                        edgeUpdates.push({ id: edgeId, hidden: false });
                        nodeUpdates.push({ id: nodeId, hidden: false });
                    } else {
                        edgeUpdates.push({ id: edgeId, hidden: true });
                        nodeUpdates.push({ id: nodeId, hidden: true });
                    }
                }
            }

            // Update the button for moving by score.
            moveByScoreButton.innerHTML = 'Move to next most relevant to query'
        }

        // Update the main network with all the changes.
        mainNetwork.body.data.edges.update(edgeUpdates);
        mainNetwork.body.data.nodes.update(nodeUpdates);

        // Set the current filter.
        currentFilterText = filterText;
    }

    // Function for handling the result of a all vertices request.
    let verticesRequestHandler = function(err, data) {
        if (err === 404) {
            console.error('Could not retrieve all graph nodes:\nThe server did not return any graph nodes.');
        } else if (err === 400) {
            console.error('Could not retrieve all graph nodes:\nThe server encountered an error while trying to send graph nodes.');
        } else if (err !== null) {
            console.error('Could not retrieve all graph nodes:\nHTTP status code was ' + err);
        } else {
            // Set the options in the Jump to dropdown.
            // These are all nodes in the current graph, even if they're not
            // connected by any chain of nodes to the currently selected node.
            let selectOptions = data.map(vertex => `<option value="${vertex.Value}">${vertex.Tag}</option>`);
            jumpToSelect.innerHTML = selectOptions.join('\n');
            dropdownVertices = data;
        }
    };

    // Function for recolouring the overview network nodes based on the current
    // main network view.
    let recolourOverviewNodes = function() {
        let nodeUpdates = [];
        if (currentCursor !== null && currentCursorId !== null) {
            for (nodeId in overviewNetwork.body.nodes) {
                let node = overviewNetwork.body.nodes[nodeId];

                // If this is the current cursor, colour it magenta.
                if (node.id === currentCursorId && node.options.label === currentCursor) {
                    nodeUpdates.push({ id: parseInt(nodeId), color: '#FF00FF' });
                // If this node is visible in the current main network view,
                // colour it aqua.
                } else if (mainNetwork.body.nodes[nodeId] !== undefined) {
                    nodeUpdates.push({ id: parseInt(nodeId), color: '#00FFFF' });
                // Otherwise, colour it light gray.
                } else {
                    nodeUpdates.push({ id: parseInt(nodeId), color: '#DDDDDD' });
                }
            }
        }
        overviewNetwork.body.data.nodes.update(nodeUpdates);
    }

    // Function for generating a graph for the main network view with supplied
    // JSON data.
    let generateGraph = function(sourceData) {

        // Clear the current main network if it exists already.
        if (mainNetwork !== null) {

            // Destroy the existing network data so we don't have the old
            // nodes and edges existing outside of the main network object.
            mainNetwork.destroy();
            mainNetwork = null;
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
            autoButton.setAttribute("style", "display: inherit");
        } else {
            document.getElementById('autoButton').setAttribute("disabled", "disabled");
            autoButton.setAttribute("style", "display: none");
        }

        // Start collecting the data for nodes.
        // Create the starting node first from the current cursor position.
        // The basic node object consists of an id and a label.
        let nodeData = [{ id: currentCursorId, label: sourceData.Vertex.Tag }];

        // Go through all the edges and add any nodes that aren't in the node
        // data yet.
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
            // The basic edge object consists of an originating node ("from")
            // and a terminating node ("to"). These should correspond to a node
            // id.
            let currentEdge = { from: edge.Start.Value, to: edge.End.Value }

            // Colour the edge red if it's pointing to the cursor, otherwise
            // colour it green.
            // When creating node and edge data, options can be defined as
            // extra properties of the object.
            if (currentEdge.to === currentCursorId) {
                currentEdge.color = 'red'
            } else {
                currentEdge.color = 'green'
            }
            edgeData.push(currentEdge);
        }

        // Set up the proper node array for visualisation.
        let nodes = new vis.DataSet(nodeData);

        // Set up the proper edge array for visualisation.
        let edges = new vis.DataSet(edgeData);

        // Select the div element to put the main network in.
        let container = document.getElementById('mainNetworkDiv');

        // Set up the data object.
        let data = {
            nodes: nodes,
            edges: edges
        };

        // Options for creating the network, that aren't attached to specific
        // nodes and edges.
        let options = {
            edges: {
                arrows: 'to', // Show arrows pointing to the "to" node.
                smooth: false // Make edges straight lines if possible.
            },
            interaction: {
                dragNodes: false, // Disable dragging nodes in the network.
                navigationButtons: true // Show buttons for altering the view of the network.
            },
            layout: {
                randomSeed: 9001 // Force a particular seed so that the graph is generated the same each time (though this is not needed as the nodes are manually positioned).
            },
            nodes: {
                shape: 'ellipse' // Set the node shape.
            },
            physics: {
                enabled: false // Prevent the nodes and edges from automatically moving around to avoid overlaps.
            }
        };

        // Initialize your network!
        mainNetwork = new vis.Network(container, data, options);

        // Move the nodes into a "wagon wheel" arrangement.
        // The cursor node should be in the centre.
        mainNetwork.moveNode(currentCursorId, 0, 0);

        // Calculate the angle between each target node, which should be even
        // between them.
        const angle = (2 * Math.PI) / (nodeData.length - 1)

        // Fixed radius for the target nodes.
        const radius = 350;

        // For every other node, position it in a circle with the given
        // radius around the central node.
        let counter = 0;
        for (row of nodeData) {
            if (row.id !== currentCursorId) {
                mainNetwork.moveNode(row.id, radius * Math.cos(counter * angle), radius * Math.sin(counter * angle));
                counter++;
            }
        }

        // Function to apply metadata to nodes.
        let applyMetadata = function(data) {
            for (row of nodeData) {
                let node = mainNetwork.body.nodes[row.id];
                // Set the "title" option of each node to the metadata.
                // This option specifies what is shown when a node is hovered over.
                node.setOptions({title: getMetadataString(row.label, data)});
            }
        };

        // Function for handling the result of a metadata request.
        let metadataRequestHandlerM = function(err, data) {
            if (err === 404) {
                console.error('Could not retrieve metadata:\nThe server did not return any metadata.');
            } else if (err === 400) {
                console.error('Could not retrieve metadata:\nThe server encountered an error while trying to send metadata.');
            } else if (err !== null) {
                console.error('Could not retrieve metadata:\nHTTP status code was ' + err);
            } else {
                applyMetadata(data);
                metadata = data;
            }
        };

        // Try to add metadata to the nodes.
        if (metadata === null) {
            getJSON('http://localhost:8080/getMetadata', metadataRequestHandlerM);
        } else {
            applyMetadata(metadata);
        }

        // Try to populate the Jump to node select.
        if (dropdownVertices === null) {
            getJSON('http://localhost:8080/getVertices', verticesRequestHandler);
        } else {
            // Set the options in the Jump to dropdown.
            // These are all nodes in the current graph, even if they're not
            // connected by any chain of nodes to the currently selected node.
            let selectOptions = dropdownVertices.map(vertex => `<option value="${vertex.Value}">${vertex.Tag}</option>`);
            jumpToSelect.innerHTML = selectOptions.join('\n');
        }

        // Apply filter if the user has entered a value and this isn't
        // a demo.
        if (demo === false) {
            let filterInput = document.getElementById('filterInput');
            let filterText = filterInput.value;
            applyFilter(filterText);
        }

        // Clicking on a main network node.
        mainNetwork.on('click', function(properties) {
            if (demo === true) return;

            // vis-network provides a properties object to find what elements
            // were involved in the event. This gives a list of IDs for the
            // nodes.
            let ids = properties.nodes;

            // Get the node objects involved in the click event.
            let clickedNodes = nodes.get(ids);

            // If there is at least one node clicked on...
            if (clickedNodes.length > 0) {

                // Unselect the nodes as we don't want to show them as
                // selected.
                mainNetwork.unselectAll();

                // Get the first clicked node (there usually should only be
                // one anyway.)
                let nextNode = clickedNodes[0];

                // We don't need to move if the node is already the cursor.
                if (nextNode.id === currentCursorId) {
                    return;
                }

                // Send a request to the server to move to the (first) node
                // that was clicked on.
                // Check if the node is one that regulates the current cursor.
                // If so, a ForceToVertex operation will be used.
                if (Object.values(mainNetwork.body.edges).find((edge) => edge.fromId === nextNode.id)) {
                    postJSON('http://localhost:8080/move', moveRequestHandler, {'moveOp':'ForceToVertex','moveInputs':{'Tag':nextNode.label,'Value':nextNode.id}});
                } else {
                    postJSON('http://localhost:8080/move', moveRequestHandler, {'moveOp':'ToVertex','moveInputs':{'Tag':nextNode.label,'Value':nextNode.id}});
                }
            }
        });

        // Display the zipper history if there is any.
        if (sourceData.History.length > 1) {
            // Clear the existing history items.
            while (historyText.firstChild) {
                historyText.removeChild(historyText.lastChild);
            }

            // Array for history entries.
            let entries = [];

            // Currently, the objects in the zipper's history are "MoveOp"
            // type in the F# code, so determining what they are takes some
            // steps.
            for (vertex of sourceData.History) {

                // Show a different entry for the first in the history.
                if (vertex === sourceData.History[sourceData.History.length - 1]) {
                    entries.unshift(`Started at ${vertex.Tag} (${vertex.Value}).`);
                } else {
                    entries.unshift(`Moved to ${vertex.Tag} (${vertex.Value}).`);
                }
            }

            // Join all the entries together as HTML list elements.
            //historyText.innerHTML = '<li>' + entries.reverse().join('</li><li>') + '</li>';
            //historyText.innerHTML = '<li>' + entries.join('</li><li>') + '</li>';

            // Create a list item with a link for each entry. Clicking on an
            // entry tells the zipper to jump to that point in the history.
            let i = 0;
            for (entry of entries) {
                let listItem = document.createElement('li');
                let itemLink = document.createElement('a');
                itemLink.setAttribute('href', '#');
                let currentI = i;
                itemLink.addEventListener('click', function (properties) {
                    if (demo === true) return;
                    postJSON('http://localhost:8080/move', moveRequestHandler, {'moveOp':'GoToHistory','moveInputs':currentI});
                });
                itemLink.innerHTML = entry;
                listItem.appendChild(itemLink);
                historyText.appendChild(listItem);
                i++;
            }

            // Add the current history location if it's not at the end.
            if (sourceData.HistoryIndex === 0 && sourceData.History.length > 1) {
                historyStatus.innerHTML = 'Currently at starting position.';
            }
            else if (sourceData.HistoryIndex < sourceData.History.length - 1) {
                historyStatus.innerHTML = `Currently at position after step ${sourceData.HistoryIndex + 1}.`;
            } else {
                historyStatus.innerHTML = '';
            }

        // Display a message if there's no history yet.
        } else {
            historyText.innerHTML = 'No navigation history.';
        }

        // Recolour the overview nodes if that visualisation exists.
        if (overviewNetwork !== null) {
            recolourOverviewNodes();
        }
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

    // Get the initial network data from the server.
    getJSON('http://localhost:8080/getGraph', graphRequestHandler);

    // Clicking on the back button.
    backButton.addEventListener('click', function() {
        if (demo === true) return;
        postJSON('http://localhost:8080/move', moveRequestHandler, {'moveOp':'Back','moveInputs':[]});
    });

    // Clicking on the forward button.
    forwardButton.addEventListener('click', function() {
        if (demo === true) return;
        postJSON('http://localhost:8080/move', moveRequestHandler, {'moveOp':'Forward','moveInputs':[]});
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

    // Clicking on the "Jump to node" button.
    jumpToButton.addEventListener('click', function() {
        if (demo === true) return;

        // Check if any options are selected in the Jump to dropdown.
        let selectedNodes = jumpToSelect.selectedOptions;

        // If there's at least one, use the first.
        if (selectedNodes.length > 0) {

            // Get the tag and the value.
            let selectedTag = selectedNodes[0].text;
            // We need to use "parseInt" here, as the actual ID is an int
            // but the key is returned as a string when we iterate.
            let selectedValue = parseInt(selectedNodes[0].value);

            // Force move to that vertex.
            postJSON('http://localhost:8080/move', moveRequestHandler, {'moveOp':'ForceToVertex','moveInputs':{'Tag':selectedTag,'Value':selectedValue}});
        }
    });

    // Clicking the "Move to most connected neighbour" button.
    moveByScoreButton.addEventListener('click', function() {
        if (demo === true) return;
        if (currentFilterText.length < 1) {
            postJSON('http://localhost:8080/move', moveRequestHandler, {'moveOp':'NextMostConnected','moveInputs':[]});
        } else {
            postJSON('http://localhost:8080/move', moveRequestHandler, {'moveOp':'NextHighestQueryScore','moveInputs':{'Property':'Synonyms','Value':currentFilterText}});
        }
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
            jumpToSelect.removeAttribute("disabled");
            jumpToButton.removeAttribute("disabled");
            moveByScoreButton.removeAttribute("disabled");
            forwardButton.removeAttribute("disabled");

            // Don't turn the auto button back on if we had an error
            // and aren't at Crp.
            if (currentCursor === "Crp") {
                autoButton.removeAttribute("disabled");
                autoButton.setAttribute("style", "display: inherit");
            }

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
            // Create a "ToVertex" MoveOp object to send to the server.
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
            // Create a "Back" MoveOp object to send to the server.
            }, {'moveOp':'Back','moveInputs':[]});
        };

        // If we're not already running a demo...
        if (demo !== true) {

            // Disable the buttons and set that we're running a demo.
            backButton.setAttribute("disabled", "disabled");
            filterButton.setAttribute("disabled", "disabled");
            autoButton.setAttribute("disabled", "disabled");
            jumpToSelect.setAttribute("disabled", "disabled");
            jumpToButton.setAttribute("disabled", "disabled");
            autoButton.setAttribute("style", "display: none");
            moveByScoreButton.setAttribute("disabled", "disabled");
            forwardButton.setAttribute("disabled", "disabled");
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

    // Function for generating an overview with supplied JSON data.
    let generateOverview = function(sourceData) {

        // Clear the current overview if it exists already.
        if (overviewNetwork !== null) {

            // Destroy the overview so we don't have the old nodes and edges
            // existing outside of the overview object.
            overviewNetwork.destroy();
            overviewNetwork = null;
        }

        // Start collecting the data for nodes.
        // Go through all the edges and add any nodes that aren't in the node
        // data yet.
        let nodeData = [];
        for (edge of sourceData) {
            if (nodeData.find((item) => item.id === edge.End.Value) === undefined) {
                nodeData.push({id: edge.End.Value, label: edge.End.Tag});
            }
            if (nodeData.find((item) => item.id === edge.Start.Value) === undefined) {
                nodeData.push({id: edge.Start.Value, label: edge.Start.Tag});
            }
        }

        // Create an array for the edge data to use later.
        let edgeData = [];
        for (edge of sourceData) {
            // The basic edge object consists of an originating node ("from")
            // and a terminating node ("to"). These should correspond to a node
            // id.
            let currentEdge = { from: edge.Start.Value, to: edge.End.Value }

            // Colour the edge red if it's pointing to the cursor, otherwise
            // colour it green.
            // When creating node and edge data, options can be defined as
            // extra properties of the object.
            if (currentEdge.to === currentCursorId) {
                currentEdge.color = 'red'
            } else {
                currentEdge.color = 'green'
            }
            edgeData.push(currentEdge);
        }

        // Set up the proper node array for visualisation.
        let nodes = new vis.DataSet(nodeData);

        // Set up the proper edge array for visualisation.
        let edges = new vis.DataSet(edgeData);

        // Select the div element to put the overview network in.
        let container = document.getElementById('overviewNetworkDiv');

        // Set up the data object.
        let data = {
            nodes: nodes,
            edges: edges
        };

        // Options for creating the network, that aren't attached to specific
        // nodes and edges.
        let options = {
            edges: {
                arrows: 'to', // Show arrows pointing to the "to" node.
                smooth: false // Make edges straight lines if possible.
            },
            interaction: {
                dragNodes: false, // Disable dragging nodes in the network.
                navigationButtons: true // Show buttons for altering the view of the network.
            },
            layout: {
                randomSeed: 9001 // Force a particular seed so that the graph is generated the same each time (though this is not needed as the nodes are manually positioned).
            },
            nodes: {
                shape: 'ellipse', // Set the node shape.
                color: '#DDDDDD' // Set the node colour to light gray.
            },
            physics: {
                enabled: false // Prevent the nodes and edges from automatically moving around to avoid overlaps.
            }
        };

        // Initialize your network!
        overviewNetwork = new vis.Network(container, data, options);
        overviewNetwork.stabilize(60);

        // Callback function to apply metadata to nodes.
        let applyMetadata = function(data) {
            for (row of nodeData) {
                let node = overviewNetwork.body.nodes[row.id];
                // Set the "title" option of each node to the metadata.
                // This option specifies what is shown when a node is hovered over.
                node.setOptions({title: getMetadataString(row.label, data)});
            }
        };

        // Function for handling the result of a metadata request.
        let metadataRequestHandlerO = function(err, data) {
            if (err === 404) {
                console.error('Could not retrieve metadata:\nThe server did not return any metadata.');
            } else if (err === 400) {
                console.error('Could not retrieve metadata:\nThe server encountered an error while trying to send metadata.');
            } else if (err !== null) {
                console.error('Could not retrieve metadata:\nHTTP status code was ' + err);
            } else {
                applyMetadata(data);
                metadata = data;
            }
        };

        // Try to add metadata to the nodes.
        if (metadata === null) {
            getJSON('http://localhost:8080/getMetadata', metadataRequestHandlerO);
        } else {
            applyMetadata(metadata);
        }

        // Clicking on a overview node.
        overviewNetwork.on('click', function(properties) {
            if (demo === true) return;

            // vis-network provides a properties object to find what elements
            // were involved in the event. This gives a list of IDs for the
            // nodes.
            let ids = properties.nodes;

            // Get the node objects involved in the click event.
            let clickedNodes = nodes.get(ids);

            // If there is at least one node clicked on...
            if (clickedNodes.length > 0) {

                // Unselect the nodes as we don't want to show them as
                // selected.
                overviewNetwork.unselectAll();

                // Get the first clicked node (there usually should only be
                // one anyway.)
                let nextNode = clickedNodes[0];

                // We don't need to move if the node is already the cursor.
                if (nextNode.id === currentCursorId) {
                    return;
                }

                // Send a request to the server to move to the (first) node
                // that was clicked on.
                // Always do a force move since we don't care whether there's a
                // connection on the graph between current and new node.
                postJSON('http://localhost:8080/move', moveRequestHandler, {'moveOp':'ForceToVertex','moveInputs':{'Tag':nextNode.label,'Value':nextNode.id}});
            }
        });

        // Recolour the current zipper cursor.
        recolourOverviewNodes();
    }

    // Function for handling the result of a graph request.
    let overviewRequestHandler = function(err, data) {
        if (err === 404) {
            console.error('Could not retrieve all graph edges:\nThe server did not return any data.');
        } else if (err === 400) {
            console.error('Could not retrieve all graph edges:\nThe server encountered an error while trying to send data.');
        } else if (err !== null) {
            console.error('Could not retrieve all graph edges:\nHTTP status code was ' + err);
        } else {
            generateOverview(data);
        }
    };

    // Call server for the edges JSON.
    getJSON('http://localhost:8080/getEdges', overviewRequestHandler);
};