const { validationResult } = require('express-validator');
const mongoose = require('mongoose');
const jwt = require('jsonwebtoken');
const config = require('./../config/config');
const bcrypt = require('bcrypt');
const crypto = require('crypto');
const { resolve } = require('path');
const saltRounds = 10;

const FavModel = require('./../models/usersFavCourseModel');
const Fav = mongoose.model('users_fav_course', FavModel);

const User = require('./../models/userModel');
const MyModel = mongoose.model('user', User);

const CourseModel = require('../models/courseModel');
const Course = mongoose.model('course', CourseModel);

const CourseTestModel = require('../models/courseTestModel');
const CourseTest = mongoose.model('coursetest', CourseTestModel);

exports.registerDataValidation = {
  email: {
    notEmpty: {
      errorMessage: 'Введите email',
      bail: true,
    },
    isEmail: {
      errorMessage: 'Неверный формат',
      bail: true,
    },
    custom: {
      errorMessage: 'Данный email уже существует',
      options: (value, { req, location, path }) => {
        return new Promise((resolve, reject) => {
          MyModel.findOne({ email: value }, (err, object) => {
            if (object !== null) {
              reject(new Error('Пользователь с таким email уже существует'));
            }

            resolve(true);
          });
        });
        //return value + req.body.foo + location + path;
      },
    },
  },
  password: {
    notEmpty: {
      errorMessage: 'Введите пароль',
      bail: true,
    },
    isLength: {
      errorMessage: 'Длина пароля должна быть не меньше 5-ти символов',
      options: { min: 5 }
    },
  }
}

exports.loginDataValidation = {
  email: {
    notEmpty: {
      errorMessage: 'Введите email',
      bail: true,
    },
    isEmail: {
      errorMessage: 'Неверный формат',
      bail: true,
    },
  },
  password: {
    notEmpty: {
      errorMessage: 'Введите пароль',
      bail: true,
    },
    custom: {
      options: (value, { req, location, path }) => {
        return new Promise((resolve, reject) => {
          MyModel.findOne({ email: req.body.email }, (err, adventure) => {
            if (adventure === null) {
              return reject(new Error('Пользователь не найден'));
            }

            bcrypt.compare(value, adventure.password, function (err, result) {
              if (result != true) {
                reject(new Error('Неверный email или пароль'));
              } else {
                req.body.refreshToken = adventure.refreshToken;
                resolve(true);
              }
            });
          });
        });
      }
    }
  }
}

exports.register = async (req, res, next) => {
  const errors = validationResult(req);
  if (!errors.isEmpty()) {
    return res.status(400).json({ errors: errors.array() });
  }

  return res.json({message: "huinya"});

  bcrypt.hash(req.body.password, saltRounds, function (err, hash) {
    const instance = new UserModel();
    instance.email = req.body.email;
    instance.password = hash;
    instance.role = 1;
    instance.refreshToken = crypto.createHash('sha256').digest('base64');
    instance.save();
    return res.json(instance);
  });
};

exports.login = async (req, res, next) => {
  const errors = validationResult(req);
  if (!errors.isEmpty()) {
    return res.status(400).json({ errors: errors.array() });
  }

  const user = await MyModel.findOne({ email: req.body.email }).select('_id email role firstName surName');

  let token = jwt.sign({
    exp: Math.floor(Date.now() / 1000) + (60),
    data: req.body.email
  }, config.jwt.secret);
  res.json({
    accessToken: token,
    refreshToken: req.body.refreshToken,
    user: user
  });
};

exports.getAllUsers = async (req, res, next) => {
  const decoded = jwt.verify(req.headers.authorization, config.jwt.secret);
  const userEmail = decoded.data;

  let select = {
    email: true,
    firstName: true,
    surName: true,
    role: true,
  };
  if (req.query.select) {
    select = JSON.parse(req.query.select);
  }

  const user = await MyModel.findOne({ email: userEmail, role: 2 });
  if (user !== null) {
    let users;
    if (req.params.uid) {
      users = await MyModel.findOne({ _id: req.params.uid }).select(select);
    } else {
      users = await MyModel.find().select(select);
    }
    res.json(users);
  } else {
    res.sendStatus(403);
  }
};

exports.getUser = async (req, res, next) => {
  const decoded = jwt.verify(req.headers.authorization, config.jwt.secret);
  const userEmail = decoded.data;

  const user = await MyModel.findOne({ email: userEmail }).select({
    _id: true,
    firstName: true,
    surName: true,
    email: true,
    role: true,
  }).populate('tests');

  if (user === null) {
    res.sendStatus(404);
  }

  res.json(user);
};

exports.putUser = async (req, res, next) => {
  if (req.params.user) {
    const user = await MyModel.updateOne({ _id: req.params.user }, req.body);

    res.json(user);
  }

  res.sendStatus(200);
};

exports.checkToken = async (req, res, next) => {
  if (req.body.refreshToken) {
    const user = await MyModel.findOne({ refreshToken: req.body.refreshToken });
    if (user !== null) {
      let token = jwt.sign({
        exp: Math.floor(Date.now() / 1000) + (60),
        data: user.email
      }, config.jwt.secret);
      res.json({
        accessToken: token
      });
    } else {
      res.json({
        message: 'Ошибка авторизации'
      });
    }
  }
};

exports.addFavCourse = async (req, res, next) => {
  if (!req.params.uid) {
    return res.status(400).json({ message: "Can't find uid param" });
  }

  const userToken = jwt.verify(req.headers.authorization, config.jwt.secret);
  const user = await MyModel.findOne({ email: userToken.data });
  if (user === null) {
    return res.status(404).json({ message: "Can't find the user" });
  }

  const checkFav = await Fav.findOne({ user_uid: user._id, course_uid: req.params.uid });
  if (checkFav !== null) {
    return res.status(400).json({ message: "Already exists" });
  }

  const fav = new Fav();
  fav.user_uid = user._id;
  fav.course_uid = req.params.uid;
  fav.save();

  return res.sendStatus(200);
}

exports.getFavCourse = async (req, res, next) => {
  if (!req.params.uid) {
    return res.status(400).json({ message: "Can't find uid param" });
  }

  const userToken = jwt.verify(req.headers.authorization, config.jwt.secret);
  const user = await MyModel.findOne({ email: userToken.data });
  if (user === null) {
    return res.status(404).json({ message: "Can't find the user" });
  }

  const fav = await Fav.findOne({ user_uid: user._id, course_uid: req.params.uid });
  if (fav === null) {
    return res.json({ fav: false });
  } else {
    return res.json({ fav: true });
  }
}

exports.delFavCourse = async (req, res, next) => {
  if (!req.params.uid) {
    return res.status(400).json({ message: "Can't find uid param" });
  }

  const userToken = jwt.verify(req.headers.authorization, config.jwt.secret);
  const user = await MyModel.findOne({ email: userToken.data });
  if (user === null) {
    return res.status(404).json({ message: "Can't find the user" });
  }

  await Fav.deleteOne({ user_uid: user._id, course_uid: req.params.uid });

  return res.sendStatus(200);
}

exports.addCourse = async (req, res, next) => {
  if (!req.params.uid) {
    return res.status(400).json({ message: "Can't find uid param" });
  }

  const userToken = jwt.verify(req.headers.authorization, config.jwt.secret);
  const user = await MyModel.findOneAndUpdate(
    { email: userToken.data },
    { $push: { subCourses: req.params.uid } },
    { new: true, useFindAndModify: false }
  );
  if (user === null) {
    return res.status(404).json({ message: "Can't find the user" });
  }

  const course = await Course.findOneAndUpdate(
    { _id: req.params.uid },
    { $push: { subUsers: user._id } },
    { new: true, useFindAndModify: false }
  );
  if (course === null) {
    return res.status(404).json({ message: "Can't find the courses" });
  }

  return res.sendStatus(200);
}

exports.delCourse = async (req, res, next) => {
  if (!req.params.uid) {
    return res.status(400).json({ message: "Can't find uid param" });
  }

  const userToken = jwt.verify(req.headers.authorization, config.jwt.secret);
  const user = await MyModel.findOneAndUpdate(
    { email: userToken.data },
    { $pull: { subCourses: req.params.uid } },
    { new: true, useFindAndModify: false }
  );
  if (user === null) {
    return res.status(404).json({ message: "Can't find the user" });
  }

  const course = await Course.findOneAndUpdate(
    { _id: req.params.uid },
    { $pull: { subUsers: user._id } },
    { new: true, useFindAndModify: false }
  );
  if (course === null) {
    return res.status(404).json({ message: "Can't find the courses" });
  }

  return res.sendStatus(200);
}

exports.getCourses = async (req, res, next) => {
  const userToken = jwt.verify(req.headers.authorization, config.jwt.secret);
  const user = await MyModel
    .findOne({ email: userToken.data })
    .populate('subCourses', 'name desc');
  if (user === null) {
    return res.status(404).json({ message: "Can't find the user" });
  }

  return res.json(user.subCourses);
}