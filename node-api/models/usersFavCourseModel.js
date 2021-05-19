const mongoose = require('mongoose');

const Schema = mongoose.Schema;
const FavCourse = new Schema({
  user_uid: { type: String },
  course_uid: { type: String },
});

module.exports = FavCourse;