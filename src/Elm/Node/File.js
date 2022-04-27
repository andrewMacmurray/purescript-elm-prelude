const fs = require("fs/promises");

exports.write_ = (filePath) => {
  return (contents) => {
    return () => {
      return fs.writeFile(filePath, contents);
    };
  };
};
