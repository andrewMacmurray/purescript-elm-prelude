import fs from "fs/promises";

export function write_(path) {
  return (contents) => {
    return () => {
      return fs.writeFile(path, contents);
    };
  };
}

export function read_(path) {
  return () => {
    return fs.readFile(path);
  };
}
