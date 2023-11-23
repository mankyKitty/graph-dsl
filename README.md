# Graph-DSL
A set of functions for creating and navigating a network graph of transcriptional regulation using the [zipper pattern](https://en.wikipedia.org/wiki/Zipper_(data_structure)).

## Building and running
Run the following commands in the repository's root folder:
```
dotnet build
dotnet run
```
When the Suave server is started, you can use a tool to send requests to the server, such as [curl](https://curl.se/) (see the examples below) or [Postman](https://www.postman.com/).

You can also open the NetworkVis.html file in a browser for a visual example of exploring a graph. This page uses vis-network.

## Example Curl commands for server
### Linux
#### ToVertex:
```
$ curl -X POST -vvv --data '{"moveOp":"ToVertex","moveInputs":{"Tag":"one","Value":1}}' http://localhost:8080/move
```
#### Back:
```
$ curl -X POST -vvv --data '{"moveOp":"Back","moveInputs":[]}' http://localhost:8080/move
```
#### MetadataSearch:
```
$ curl -X POST -vvv --data '{"moveOp":"MetadataSearch","moveInputs":{"Property":"Synonyms","Value":"child"}}' http://localhost:8080/move
```
#### MetadataSearchMulti (AND):
```
$ curl -X POST -vvv --data '{"moveOp":"MetadataSearchMulti","moveInputs":{"Operation":"AND","Queries":[{"Property":"Synonyms","Value":"child"},{"Property":"Name","Value":"two"}]}}' http://localhost:8080/move
```
#### MetadataSearchMulti (OR):
```
$ curl -X POST -vvv --data '{"moveOp":"MetadataSearchMulti","moveInputs":{"Operation":"OR","Queries":[{"Property":"Synonyms","Value":"child"},{"Property":"Name","Value":"two"}]}}' http://localhost:8080/move
```
#### NextMostConnected:
```
$ curl -X POST -vvv --data '{"moveOp":"NextMostConnected","moveInputs":[]}' http://localhost:8080/move
```
#### Forward:
```
$ curl -X POST -vvv --data '{"moveOp":"Forward","moveInputs":[]}' http://localhost:8080/move
```
#### GoToHistory:
```
$ curl -X POST -vvv --data '{"moveOp":"GoToHistory","moveInputs":0}' http://localhost:8080/move
```
#### NextHighestQueryScore:
```
$ curl -X POST -vvv --data '{"moveOp":"NextHighestQueryScore","moveInputs":{"Property":"Synonyms","Value":"child"}}' http://localhost:8080/move
```
#### NextHighestScore:
```
$ curl -X POST -vvv --data '{"moveOp":"NextHighestScore","moveInputs":[]}' http://localhost:8080/move
```

### Windows
#### ToVertex:
```
curl -X POST -vvv --data {\"moveOp\":\"ToVertex\",\"moveInputs\":{\"Tag\":\"one\",\"Value\":1}} http://localhost:8080/
```
#### Back
```
curl -X POST -vvv --data {\"moveOp\":\"Back\",\"moveInputs\":[]} http://localhost:8080/move
```
#### MetadataSearch:
```
curl -X POST -vvv --data {\"moveOp\":\"MetadataSearch\",\"moveInputs\":{\"Property\":\"Synonyms\",\"Value\":\"child\"}} http://localhost:8080/move
```
#### MetadataSearchMulti (AND):
```
curl -X POST -vvv --data {\"moveOp\":\"MetadataSearchMulti\",\"moveInputs\":{\"Operation\":\"AND\",\"Queries\":[{\"Property\":\"Synonyms\",\"Value\":\"child\"},{\"Property\":\"Name\",\"Value\":\"two\"}]}} http://localhost:8080/
```
#### MetadataSearchMulti (OR):
```
curl -X POST -vvv --data {\"moveOp\":\"MetadataSearchMulti\",\"moveInputs\":{\"Operation\":\"OR\",\"Queries\":[{\"Property\":\"Synonyms\",\"Value\":\"child\"},{\"Property\":\"Name\",\"Value\":\"two\"}]}} http://localhost:8080/move
```
#### NextMostConnected
```
curl -X POST -vvv --data {\"moveOp\":\"NextMostConnected\",\"moveInputs\":[]} http://localhost:8080/move
```
#### Forward
```
curl -X POST -vvv --data {\"moveOp\":\"Forward\",\"moveInputs\":[]} http://localhost:8080/move
```
#### GoToHistory
```
curl -X POST -vvv --data {\"moveOp\":\"GoToHistory\",\"moveInputs\":0} http://localhost:8080/move
```
#### NextHighestQueryScore
```
curl -X POST -vvv --data {\"moveOp\":\"NextHighestQueryScore\",\"moveInputs\":{\"Property\":\"Synonyms\",\"Value\":\"child\"}} http://localhost:8080/move
```
#### NextHighestScore
```
curl -X POST -vvv --data {\"moveOp\":\"NextHighestScore\",\"moveInputs\":[]} http://localhost:8080/move
```