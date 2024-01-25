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

## Example curl commands
For a full list of possible commands, see the "Server commands" section in the [wiki](https://github.com/mankyKitty/graph-dsl/wiki).

### Viewing the current cursor
#### Linux:
```
$ curl -X GET -vvv http://localhost:8080/getLocation
```
#### Windows:
```
curl -X GET -vvv http://localhost:8080/getLocation
```

### Viewing the destinations from the current cursor
#### Linux:
```
$ curl -X GET -vvv http://localhost:8080/getDestinations
```
#### Windows:
```
curl -X GET -vvv http://localhost:8080/getDestinations
```

### Moving to a new vertex
#### Linux (example):
```
$ curl -X POST -vvv --data '{"moveOp":"ToVertex","moveInputs":{"Tag":"one","Value":1}}' http://localhost:8080/move
```
#### Windows (example):
```
curl -X POST -vvv --data {\"moveOp\":\"ToVertex\",\"moveInputs\":{\"Tag\":\"one\",\"Value\":1}} http://localhost:8080/move
```

### Moving backwards in history
#### Linux:
```
$ curl -X POST -vvv --data '{"moveOp":"Back","moveInputs":[]}' http://localhost:8080/move
```
#### Windows:
```
curl -X POST -vvv --data {\"moveOp\":\"Back\",\"moveInputs\":[]} http://localhost:8080/move
```

### Moving forwards in history
#### Linux:
```
$ curl -X POST -vvv --data '{"moveOp":"Forward","moveInputs":[]}' http://localhost:8080/move
```
#### Windows:
```
curl -X POST -vvv --data {\"moveOp\":\"Forward\",\"moveInputs\":[]} http://localhost:8080/move
```