'use strict';

var program = require('./elm/Main.elm');

var tokenField = 'token'

var app = program.Elm.Main.init({
    flags: localStorage.getItem(tokenField)
});


app.ports.saveToken.subscribe(function(token) {
    localStorage.setItem(tokenField, token);
});

app.ports.removeToken.subscribe(function() {
    console.log("das")
    localStorage.removeItem(tokenField);
});
