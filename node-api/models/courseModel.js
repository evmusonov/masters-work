const mongoose = require('mongoose');

const Schema = mongoose.Schema;
const Course = new Schema({
  name: { type: String },
  desc: { type: String },
  onto: { type: Schema.Types.ObjectId, ref: "onto" },
  vis: { type: Number },
  del: { type: Number, default: 0 },
  subUsers: [{ type: Schema.Types.ObjectId, ref: "user" }],
  tests: [{ type: Schema.Types.ObjectId, ref: "coursetest" }],
});

module.exports = Course;