const mongoose = require('mongoose');

const Schema = mongoose.Schema;
const Onto = new Schema({
  name: { type: String },
  cnl: { type: String },
  sub_list: { type: Array },
  creator_uid: { type: String },
  vis: { type: Number },
  del: { type: Number, default: 0 },
});

module.exports = Onto;