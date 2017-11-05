"use strict";

exports._get = function(name) {
    return document.head.querySelector("[name=" + name + "]").content;
};
