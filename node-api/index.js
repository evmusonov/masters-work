var express = require("express");
const router = require('./routes');
var app = express();

app.use(express.json()) // for parsing application/json
app.use(express.urlencoded({ extended: true })) // for parsing application/x-www-form-urlencoded
app.use('/api', router);

app.listen(8070, () => {
});
