var express = require('express');
var app = express();

app.get('/', function(req,res){
  res.sendFile(__dirname + '/index.html');
})

app.get('/api/users/:userName', function(req,res){
  var userName = req.params.userName;
  res.sendFile(__dirname + '/users/' + userName + '.json');
});

app.use('/css', express.static('css'));

app.use('/dist', express.static('dist'));

app.listen(3000, function(){
  console.log('Example app listening on port 3000');
})
