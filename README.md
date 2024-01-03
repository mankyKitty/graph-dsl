# Graph-DSL
A set of functions for creating and navigating a network graph of transcriptional regulation using the [zipper pattern](https://en.wikipedia.org/wiki/Zipper_(data_structure)).

This library is intended to follow functional principles as much as possible, such as avoiding mutable values and side effects, and allowing currying and piping.

## Building and running
Run the following commands in the repository's root folder:
```
dotnet build
dotnet run
```
When the Suave server is started, you can use a tool to send requests to the server, such as [curl](https://curl.se/) (see the examples below) or [Postman](https://www.postman.com/).

You can also open the NetworkVis.html file in a browser for a visual example of exploring a graph. This page uses vis-network.

The following symbols can be set while building:
- LOGGING: Produces output to the console for debugging purposes, including the time taken for some operations.
- VERBOSE: Produces additional debugging output.

## Example Curl commands for server
### ToVertex
Moves to the specified vertex if it's connected to the current location of the zipper.

#### Required move input:
- Object containing the following properties:
	- Tag: string value representing the tag of the vertex.
	- Value: integer value representing the value of the vertex.
#### Linux example:
```
$ curl -X POST -vvv --data '{"moveOp":"ToVertex","moveInputs":{"Tag":"one","Value":1}}' http://localhost:8080/move
```
#### Windows example:
```
curl -X POST -vvv --data {\"moveOp\":\"ToVertex\",\"moveInputs\":{\"Tag\":\"one\",\"Value\":1}} http://localhost:8080/
```
### Back
Moves to the previous position in the zipper's history, if not at the start of the history. History after this point is not removed until a new command is sent (while the zipper is not at the end of the history).

#### Required move input:
- An empty array.
#### Linux example:
```
$ curl -X POST -vvv --data '{"moveOp":"Back","moveInputs":[]}' http://localhost:8080/move
```
#### Windows example:
```
curl -X POST -vvv --data {\"moveOp\":\"Back\",\"moveInputs\":[]} http://localhost:8080/move
```
### MetadataSearch
Moves to the first vertex connected to the current location of the zipper whose metadata meets the specified condition.

#### Required move input:
- Object containing the following properties:
	- Property: string value representing the property of the metadata to match against.
	- Value: string value representing the term to find in the specified metadata property (is not case sensitive).
#### Linux example:
```
$ curl -X POST -vvv --data '{"moveOp":"MetadataSearch","moveInputs":{"Property":"Synonyms","Value":"child"}}' http://localhost:8080/move
```
#### Windows example:
```
curl -X POST -vvv --data {\"moveOp\":\"MetadataSearch\",\"moveInputs\":{\"Property\":\"Synonyms\",\"Value\":\"child\"}} http://localhost:8080/move
```
### MetadataSearchMulti (AND)
Moves to the first vertex connected to the current location of the zipper whose metadata meets all of the specified conditions.

#### Required move input:
- Object containing the following properties:
	- Operation with the string value "AND" to specify that all conditions must be true.
	- Queries: An array of objects consisting of the following properties:
		- Property: string value representing the property of the metadata to match against.
		- Value: string value representing the term to find in the specified metadata property (is not case sensitive).
#### Linux example:
```
$ curl -X POST -vvv --data '{"moveOp":"MetadataSearchMulti","moveInputs":{"Operation":"AND","Queries":[{"Property":"Synonyms","Value":"child"},{"Property":"Name","Value":"two"}]}}' http://localhost:8080/move
```
#### Windows example:
```
curl -X POST -vvv --data {\"moveOp\":\"MetadataSearchMulti\",\"moveInputs\":{\"Operation\":\"AND\",\"Queries\":[{\"Property\":\"Synonyms\",\"Value\":\"child\"},{\"Property\":\"Name\",\"Value\":\"two\"}]}} http://localhost:8080/
```
### MetadataSearchMulti (OR)
Moves to the first vertex connected to the current location of the zipper whose metadata meets at least one of the specified conditions.

#### Required move input:
- Object containing the following properties:
	- Operation with the string value "OR" to specify that at least one condition must be true.
	- Queries: An array of objects consisting of the following properties:
		- Property: string value representing the property of the metadata to match against.
		- Value: string value representing the term to find in the specified metadata property (is not case sensitive).
#### Linux example:
```
$ curl -X POST -vvv --data '{"moveOp":"MetadataSearchMulti","moveInputs":{"Operation":"OR","Queries":[{"Property":"Synonyms","Value":"child"},{"Property":"Name","Value":"two"}]}}' http://localhost:8080/move
```
#### Windows example:
```
curl -X POST -vvv --data {\"moveOp\":\"MetadataSearchMulti\",\"moveInputs\":{\"Operation\":\"OR\",\"Queries\":[{\"Property\":\"Synonyms\",\"Value\":\"child\"},{\"Property\":\"Name\",\"Value\":\"two\"}]}} http://localhost:8080/move
```
### NextMostConnected
Moves to the vertex connected to the current location of the zipper that has the most outgoing connections. Whether or not the vertex must not be in the history yet can be specified.

#### Required move input:
- Boolean value representing whether to restrict the movement to unvisited vertices.
#### Linux example:
```
$ curl -X POST -vvv --data '{"moveOp":"NextMostConnected","moveInputs":true}' http://localhost:8080/move
```
#### Windows example:
```
curl -X POST -vvv --data {\"moveOp\":\"NextMostConnected\",\"moveInputs\":true} http://localhost:8080/move
```
### Forward
Moves to the next position in the zipper's history, if not at the end of the history.

#### Required move input:
- An empty array.
#### Linux example:
```
$ curl -X POST -vvv --data '{"moveOp":"Forward","moveInputs":[]}' http://localhost:8080/move
```
#### Windows example:
```
curl -X POST -vvv --data {\"moveOp\":\"Forward\",\"moveInputs\":[]} http://localhost:8080/move
```
### GoToHistory
Moves to the specified index in the zipper's history, if it exists. The beginning of the history is index 0.

#### Required move input:
- Integer value representing the index in the history.
#### Linux example:
```
$ curl -X POST -vvv --data '{"moveOp":"GoToHistory","moveInputs":0}' http://localhost:8080/move
```
#### Windows example:
```
curl -X POST -vvv --data {\"moveOp\":\"GoToHistory\",\"moveInputs\":0} http://localhost:8080/move
```
### NextHighestQueryScore
Moves along the outgoing edge connected to the current zipper location that has the highest score value, using a weighted score calculated using the specified metadata condition, with a vertex that has not yet been visted in the history.

#### Required move input:
- Object containing the following properties:
	- Property: string value representing the property of the metadata to match against.
	- Value: string value representing the term to find in the specified metadata property (is not case sensitive).
#### Linux example:
```
$ curl -X POST -vvv --data '{"moveOp":"NextHighestQueryScore","moveInputs":{"Property":"Synonyms","Value":"child"}}' http://localhost:8080/move
```
#### Windows example:
```
curl -X POST -vvv --data {\"moveOp\":\"NextHighestQueryScore\",\"moveInputs\":{\"Property\":\"Synonyms\",\"Value\":\"child\"}} http://localhost:8080/move
```
### NextHighestScore
Moves along the outgoing edge connected to the current zipper location that has the highest score value. Whether or not the vertex must not be in the history yet can be specified.

#### Required move input:
- Boolean value representing whether to restrict the movement to unvisited vertices.
#### Linux example:
```
$ curl -X POST -vvv --data '{"moveOp":"NextHighestScore","moveInputs":true}' http://localhost:8080/move
```
#### Windows example:
```
curl -X POST -vvv --data {\"moveOp\":\"NextHighestScore\",\"moveInputs\":true} http://localhost:8080/move
```

## Scoring algorithms
To assist the user in navigating the network, as well as exploring automated navigation, there are several functions available to set edge scores. Some of these use a weighted scoring that considers verticies further away than the target vertex on that edge, up to (by default in the code) four steps away. This can allow, for example, scoring an edege highly because a large number of connections appears several steps past the initial target vertex.

Currently, this is available for scoring based on the number of outgoing connections of each vertex, and the number of times a given metadata condition is met on the vertices. Currently, functions that use depth first search (using a custom recursive function) and breadth first search (using QuikGraph's algorithm functions) are available.

All scoring functions operate on a deep clone of the graph, so that the existing scores on the graph are not overwritten if the new score is only needed temporarily.