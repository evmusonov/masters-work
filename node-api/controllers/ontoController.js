const { validationResult } = require('express-validator');
const mongoose = require('mongoose');
const OntoModel = require('../models/ontoModel');
const Onto = mongoose.model('onto', OntoModel);

exports.addDataValidation = {
  name: {
    notEmpty: {
      errorMessage: 'Введите название',
      bail: true,
    },
    custom: {
      errorMessage: 'Данная онтология уже существует',
      options: (value, { req, location, path }) => {
        return new Promise((resolve, reject) => {
          Onto.findOne({ name: value, del: 0 }, (err, object) => {
            if (object !== null) {
              reject(new Error('Данная онтология уже существует'));
            }

            resolve(true);
          });
        });
      },
    },
  },
}

exports.add = (req, res, next) => {
  const errors = validationResult(req);
  if (!errors.isEmpty()) {
    return res.status(400).json({ errors: errors.array() });
  }

  let framesArray = [];
  for(let subject in req.body.sub_list) {
    framesArray.push({name: req.body.sub_list[subject]});
  }

  const onto = new Onto();
  onto.name = req.body.name;
  onto.cnl = req.body.cnl;
  onto.sub_list = framesArray;
  onto.vis = 0;
  onto.save();

  return res.sendStatus(200);
}

exports.get = async (req, res, next) => {
  let ontos = [];
  let where = null;
  if (req.query.where) {
    where = JSON.parse(req.query.where);
  }

  if (req.params.uid) {
    if (!where) {
      where = { _id: req.params.uid };
    } else {
      where._id = req.params.uid;
    }
    ontos = await Onto.findOne(where).select({
      _id: true,
      name: true,
      cnl: true,
      sub_list: true,
      vis: true,
    });
  } else {
    if (!where) {
      where = { del: 0 };
    }
    ontos = await Onto.find(where).select({
      _id: true,
      name: true,
      cnl: true,
      sub_list: true,
      vis: true,
      del: true,
    });
  }

  return res.json(ontos);
}

exports.put = async (req, res, next) => {
  if (req.params.uid) {
    let framesArray = [];
    for(let subject in req.body.sub_list) {
      framesArray.push({name: req.body.sub_list[subject]});
    }

    req.body.sub_list = framesArray;

    await Onto.updateOne({ _id: req.params.uid }, req.body);
    return res.sendStatus(200);
  }

  return res.status(500).json({message: "Something went wrong"});
}

exports.delete = async (req, res, next) => {
  if (req.params.uid) {
    await Onto.updateOne({ _id: req.params.uid }, { del: 1 });
    return res.sendStatus(200);
  }

  return res.status(500).json({message: "Something went wrong"});
}