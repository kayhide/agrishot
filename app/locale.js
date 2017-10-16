"use strict";

module.exports.translations = (locale) => {
  return require(`./locale/${locale}`);
};
