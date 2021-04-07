const { validationResult } = require('express-validator');
const mongoose = require('mongoose');
const User = require('./../models/userModel');
const jwt = require('jsonwebtoken');
const config = require('./../config/config');
const bcrypt = require('bcrypt');
const crypto = require('crypto');
const { resolve } = require('path');
const saltRounds = 10;

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
        const MyModel = mongoose.model('user', User);
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
        const UserModel = mongoose.model('user', User);
        return new Promise((resolve, reject) => {
          UserModel.findOne({ email: req.body.email }, (err, adventure) => {
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

exports.register = (req, res, next) => {
  const errors = validationResult(req);
  if (!errors.isEmpty()) {
    return res.status(400).json({ errors: errors.array() });
  }

  bcrypt.hash(req.body.password, saltRounds, function (err, hash) {
    const UserModel = mongoose.model('user', User);
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

  const MyModel = mongoose.model('user', User);
  const user = await MyModel.findOne({ email: req.body.email }).select('_id email role');

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

exports.get = (req, res, next) => {

  //const MyModel = mongoose.model('user', User);
  //const mycase = await MyModel.findOne({ email: 'ev@ya.ru' });

  res.json('asd');
};

exports.getUser = async (req, res, next) => {
  const decoded = jwt.verify(req.headers.authorization, config.jwt.secret);
  const userEmail = decoded.data;

  const MyModel = mongoose.model('user', User);
  const user = await MyModel.findOne({ email: userEmail }).select({
    _id: true,
    firstName: true,
    surName: true,
    email: true,
    role: true,
  });

  if (user === null) {
    res.sendStatus(404);
  }

  res.json(user);
};

exports.putUser = async (req, res, next) => {
  if (req.params.user) {
    const MyModel = mongoose.model('user', User);
    const user = await MyModel.updateOne({ _id: req.params.user }, req.body);

    res.json(user);
  }

  res.sendStatus(200);
};

exports.checkToken = async (req, res, next) => {
  if (req.body.refreshToken) {
    const MyModel = mongoose.model('user', User);
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