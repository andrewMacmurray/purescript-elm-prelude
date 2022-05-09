const fs = require("fs/promises");

exports.write_ = (path) => {
  return (contents) => {
    return () => {
      return fs.writeFile(path, contents);
    };
  };
};

exports.read_ = (path) => {
  return () => {
    return fs.readFile(path);
  };
};
