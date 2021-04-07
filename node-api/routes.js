var express = require('express');
var router = express.Router();
const mongoose = require('mongoose');

var NamedRouter = require('named-routes');
var namedRouter = new NamedRouter();
namedRouter.extendExpress(router);

const { checkSchema, body } = require('express-validator');
const jwt = require('jsonwebtoken');
const config = require('./config/config');
const userController = require('./controllers/userController');


async function start() {
  await mongoose.connect('mongodb://mongo/intes', {
    "auth": { "authSource": "admin" },
    "user": "root",
    "pass": "root",
    useNewUrlParser: true,
    useUnifiedTopology: true,
    useFindAndModify: false,
    useCreateIndex: true
  });
}

const protected = (req, res, next) => {
  if (!req.headers['authorization']) {
    return res.status(403).send({message: 'Не авторизован'});
  }
  try {
    let decoded = jwt.verify(req.headers['authorization'], config.jwt.secret);
  } catch(err) {
    return res.status(401).send({message: err});
  }

  next();
} 

router.use(function (req, res, next) { 
  start()
  next()
})

router.post(
  '/user/register',
  'register',
  checkSchema(userController.registerDataValidation),
  userController.register,
);

router.post(
  '/user/login',
  'login',
  checkSchema(userController.loginDataValidation),
  userController.login,
);

router.get(
  '/users',
  'user-list',
  protected,
  userController.get,
);

router.get(
  '/user',
  'get-user',
  protected,
  userController.getUser,
);

router.put(
  '/user/:user',
  'put-user',
  protected,
  userController.putUser,
);

router.post(
  '/check-token',
  'check-token',
  userController.checkToken,
);

module.exports = router