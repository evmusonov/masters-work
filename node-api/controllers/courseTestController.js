const { validationResult } = require('express-validator');
const mongoose = require('mongoose');
const jwt = require('jsonwebtoken');
const config = require('./../config/config');

const CourseModel = require('../models/courseModel');
const Course = mongoose.model('course', CourseModel);
const UserModel = require('../models/userModel');
const User = mongoose.model('user', UserModel);
const CourseTestModel = require('../models/courseTestModel');
const CourseTest = mongoose.model('courseTest', CourseTestModel);

exports.addDataValidation = {
  course_uid: {
    notEmpty: {
      errorMessage: 'Пустое значение',
      bail: true,
    },
  },
  sub_list: {
    notEmpty: {
      errorMessage: 'Пустое значение',
      bail: true,
    },
  },
}

exports.getDataValidation = {
  course: {
    in: ['params'],
    notEmpty: {
      errorMessage: 'Пустое значение',
      bail: true,
    },
  },
}

exports.add = async (req, res, next) => {
  const errors = validationResult(req);
  if (!errors.isEmpty()) {
    return res.status(400).json({ errors: errors.array() });
  }

  const userToken = jwt.verify(req.headers.authorization, config.jwt.secret);
  const user = await User.findOne(
    { email: userToken.data },
  );
  if (user === null) {
    return res.status(404).json({ message: "Can't find the user" });
  }

  let courseTest = await CourseTest.findOne({ course: req.body.course_uid, user: user._id });
  if (courseTest === null) {
    let sortedSubList = req.body.sub_list.sort(() => Math.random() - 0.5);
    courseTest = new CourseTest();
    courseTest.course = req.body.course_uid;
    courseTest.user = user._id;
    courseTest.sub_list = sortedSubList;
    courseTest.vis = 0;
    courseTest.save();

    const course = await Course.findOneAndUpdate(
      { _id: req.body.course_uid },
      { $push: { tests: courseTest._id } },
      { new: true, useFindAndModify: false }
    );
    if (course === null) {
      return res.status(404).json({ message: "Can't find the course" });
    }

    await User.findOneAndUpdate(
      { _id: user._id },
      { $push: { tests: courseTest._id } },
      { new: true, useFindAndModify: false }
    );

    return res.json(courseTest);
  }

  return res.sendStatus(400).json({ message: "Already exists" });
}

exports.get = async (req, res, next) => {
  const errors = validationResult(req);
  if (!errors.isEmpty()) {
    return res.status(400).json({ errors: errors.array() });
  }

  const userToken = jwt.verify(req.headers.authorization, config.jwt.secret);
  const user = await User.findOne(
    { email: userToken.data },
  );
  if (user === null) {
    return res.status(404).json({ message: "Can't find the user" });
  }

  let courseTest = await CourseTest.findOne({ course: req.params.course, user: user._id })
    .populate({
      path: "course",
      populate: {
        path: "onto"
      }
    });
  if (courseTest === null) {
    res.status(404).json({ message: "The item is not found" });
  }

  return res.json(courseTest);
}

exports.putAnswer = async (req, res, next) => {
  const errors = validationResult(req);
  if (!errors.isEmpty()) {
    return res.status(400).json({ errors: errors.array() });
  }

  const userToken = jwt.verify(req.headers.authorization, config.jwt.secret);
  const user = await User.findOne(
    { email: userToken.data },
  );
  if (user === null) {
    return res.status(404).json({ message: "Can't find the user" });
  }

  const checkExist = await CourseTest.findOne({ "answers.sub": req.body.sub });
  if (checkExist === null) {
    await CourseTest.findOneAndUpdate(
      { course: req.params.course, user: user._id },
      { $push: { answers: { sub: req.body.sub, answer: req.body.answer } } },
      { new: true, useFindAndModify: true }
    );
  }

  return res.sendStatus(200);
}