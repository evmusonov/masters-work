const mongoose = require('mongoose');

const Schema = mongoose.Schema;
const User = new Schema({
  firstName: { type: String },
  surName: { type: String },
  email: { type: String },
  password: { type: String, min: 5 },
  role: { type: Number },
  refreshToken: { type: String }
});

module.exports = User;