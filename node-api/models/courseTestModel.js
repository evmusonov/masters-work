const mongoose = require('mongoose');

const Schema = mongoose.Schema;
const Test = new Schema({
  vis: { type: Number },
  del: { type: Number, default: 0 },
  course: { type: Schema.Types.ObjectId, ref: "course" },
  user: { type: Schema.Types.ObjectId, ref: "user" },
  sub_list: { type: Array },
  answers: { type: Array }
});

module.exports = Test;