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
const courseController = require('./controllers/courseController');
const ontoController = require('./controllers/ontoController');
const courseTestController = require('./controllers/courseTestController');


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
  '/users/register',
  'register',
  checkSchema(userController.registerDataValidation),
  userController.register,
);

router.post(
  '/users/login',
  'login',
  checkSchema(userController.loginDataValidation),
  userController.login,
);

router.get(
  '/user',
  'get-user',
  protected,
  userController.getUser,
);
router.get(
  '/users(/:uid)?',
  'get-all-users',
  protected,
  userController.getAllUsers,
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

// COURSE
router.post(
  '/courses',
  'add-admin-courses',
  protected,
  checkSchema(courseController.addDataValidation),
  courseController.add,
);

router.get(
  '/courses(/:uid)?',
  'get-admin-courses',
  //protected,
  courseController.get,
);

router.put(
  '/courses/:uid',
  'put-admin-courses',
  protected,
  checkSchema(courseController.addDataValidation),
  courseController.put,
);

router.delete(
  '/courses/:uid',
  'delete-admin-courses',
  protected,
  courseController.delete,
);

// FAVS COURSES
router.post(
  '/users/favs/courses/:uid',
  'add-users-favs-courses',
  protected,
  userController.addFavCourse,
);

router.get(
  '/users/favs/courses/:uid',
  'get-users-favs-courses',
  protected,
  userController.getFavCourse,
);

router.delete(
  '/users/favs/courses/:uid',
  'del-users-favs-courses',
  protected,
  userController.delFavCourse,
);

// ONTO
router.post(
  '/ontos',
  'add-admin-ontos',
  protected,
  checkSchema(ontoController.addDataValidation),
  ontoController.add,
);

router.get(
  '/ontos(/:uid)?',
  'get-admin-ontos',
  //protected,
  ontoController.get,
);

router.put(
  '/ontos/:uid',
  'put-admin-ontos',
  protected,
  checkSchema(ontoController.addDataValidation),
  ontoController.put,
);

router.delete(
  '/ontos/:uid',
  'delete-admin-ontos',
  protected,
  ontoController.delete,
);

// USER2COURSE
router.post(
  '/users/courses/:uid',
  'add-user-course',
  protected,
  userController.addCourse,
);
router.delete(
  '/users/courses/:uid',
  'del-user-course',
  protected,
  userController.delCourse,
);
router.get(
  '/users/courses',
  'get-user-courses',
  protected,
  userController.getCourses,
);
router.get(
  '/users/courses-new',
  'get-user-new-courses',
  userController.getNewCourses,
);

// COURSE TESTS
router.post(
  '/users/courses/:uid/tests',
  'add-user-course-test',
  protected,
  checkSchema(courseTestController.addDataValidation),
  courseTestController.add,
);
router.get(
  '/users/courses/:course/tests',
  'get-user-course-test',
  protected,
  checkSchema(courseTestController.getDataValidation),
  courseTestController.get,
);
router.put(
  '/users/courses/:course/tests/:test/answers',
  'put-user-course-test',
  protected,
  //checkSchema(courseTestController.getDataValidation),
  courseTestController.putAnswer,
);

module.exports = router