const { validationResult } = require('express-validator');
const mongoose = require('mongoose');
const CourseModel = require('../models/courseModel');
const Course = mongoose.model('course', CourseModel);

exports.addDataValidation = {
  name: {
    notEmpty: {
      errorMessage: 'Введите название',
      bail: true,
    },
    custom: {
      errorMessage: 'Данный курс уже существует',
      options: (value, { req, location, path }) => {
        return new Promise((resolve, reject) => {
          Course.findOne({ name: value, del: 0 }, (err, object) => {
            if (object !== null) {
              reject(new Error('Данный курс уже существует'));
            }

            resolve(true);
          });
        });
      },
    },
  },
  desc: {
    notEmpty: {
      errorMessage: 'Укажите описание',
      bail: true,
    },
  },
  onto_uid: {
    notEmpty: {
      errorMessage: 'Выберите онтологию',
      bail: true,
    },
  },
}

exports.add = (req, res, next) => {
  const errors = validationResult(req);
  if (!errors.isEmpty()) {
    return res.status(400).json({ errors: errors.array() });
  }

  const course = new Course();
  course.name = req.body.name;
  course.desc = req.body.desc;
  course.onto = req.body.onto_uid;
  course.vis = 0;
  course.save();

  return res.sendStatus(200);
}

exports.get = async (req, res, next) => {
  let courses = [];
  let where = null;
  let options = null;
  if (req.query.where) {
    where = JSON.parse(req.query.where);
  }
  if (req.query.options) {
    options = JSON.parse(req.query.options);
  }

  if (req.params.uid) {
    if (!where) {
      where = { _id: req.params.uid };
    } else {
      where._id = req.params.uid;
    }
    courses = await Course.findOne(where).select({
      _id: true,
      name: true,
      desc: true,
      onto: true,
      subUsers: true,
      vis: true,
    }).populate('onto');
  } else {
    if (!where) {
      where = { del: 0 };
    }
    courses = await Course.find(where).select({
      _id: true,
      name: true,
      desc: true,
      onto: true,
      subUsers: true,
      vis: true,
      del: true,
    });
  }

  return res.json(courses);
}

exports.put = async (req, res, next) => {
  if (req.params.uid) {
    await Course.updateOne({ _id: req.params.uid }, req.body);
    return res.sendStatus(200);
  }

  return res.status(500).json({message: "Something went wrong"});
}

exports.delete = async (req, res, next) => {
  if (req.params.uid) {
    await Course.updateOne({ _id: req.params.uid }, { del: 1 });
    return res.sendStatus(200);
  }

  return res.status(500).json({message: "Something went wrong"});
}