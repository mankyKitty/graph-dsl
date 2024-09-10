# Graph-DSL
A set of functions for creating and navigating a network graph of transcriptional regulation using the [zipper pattern](https://en.wikipedia.org/wiki/Zipper_(data_structure)).

This library is intended to follow functional principles as much as possible, such as avoiding mutable values and side effects, and allowing currying and piping.

## Building and running
As a .NET project, this library requires [.NET 6.0](https://dotnet.microsoft.com/en-us/download/dotnet/6.0). If you have it installed, run the command `dotnet build` in the repository's root folder to build the library.

This project also includes a small example program to demonstrate how the zipper can be used, a [Suave](http://suave.io/) server that allows navigation over a tiny graph. Use the command `dotnet run` to start the server. You can then use a tool to send requests to the server, such as [curl](https://curl.se/) (see the examples below) or [Postman](https://www.postman.com/), or open the NetworkVis.html file in a browser for a visual example of exploring a graph.

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