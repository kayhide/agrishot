"use strict";

module.exports.translations = (locale) => {
  return require(`app/locale/${locale}`);
};
