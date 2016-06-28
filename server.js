var express = require('express');
var app = express();
idGenerator = require('./id-generator'),
log = require('./logger'),
db = require('./graph');


app.get('/', function(req,res){
  res.sendFile(__dirname + '/index.html');
})

/*
app.get('/api/users/:userName', function(req,res){
  var userName = req.params.userName;
  res.sendFile(__dirname + '/users/' + userName + '.json');
});
*/

app.get('/api/:userId', function(req, res) {
  const userId = req.params.userId;

  db.getUserInfo2(userId)
    .then(userInfo => res.send(userInfo).end())
    .catch(error => {
      log.info(error);
      res.status(404).end('Unknown user');
    });
});


app.get('/api/:userId/getLocation', (req,res) => {
  const userId = req.params.userId;
  log.info(userId);
  db.getLocationById(userId)
    .then(location => res.send(location).end());
});

app.post('/api/:userId/postLocation', (req,res) => {
  const {locationName, country, city, postal} = req.body,
    userId = req.params.userId;

  db.connectUserToLocation(userId, locationName, country, city, postal)
    .then(() => res.send(req.body).end())
    .catch(error => {
      log.info(error);
      res.status(500).end('server error!');
    });
});

app.delete('/api/:userId/:locationId/deleteLocation', (req,res) =>{
  const
    locationId = req.params.locationId;

  db.removeLocation(locationId)
    .then(() => res.send().end())
    .catch(error => {
      log.info(error);
      res.status(500).end('server error!');
    });
});

app.post('/api/:userId/:locationId/updateLocation', (req,res) =>{
  const {locationName, country, city, postal} = req.body,
    userId = req.params.userId,
    locationId = req.params.locationId;

  db.updateLocation(locationId, userId, locationName, country, city, postal)
    .then(() => res.send().end())
    .catch(error => {
      log.info(error);
      res.status(500).end('server error!');
    });
});

app.use('/css', express.static('css'));

app.use('/dist', express.static('dist'));

app.listen(3000, function(){
  console.log('Example app listening on port 3000');
})
