'use strict';

var program = require('./elm/Main.elm');

var app = program.Elm.Main.init({
    flags: localStorage.getItem('token')
});


app.ports.saveToken.subscribe(function(token) {
    localStorage.setItem('token', token);
});
