exports.string_ = string;

exports.int_ = int;

exports.optionalString_ = function (key) {
  return function (default_) {
    try {
      return string(key);
    } catch (e) {
      return default_;
    }
  };
};

exports.optionalInt_ = function (key) {
  return function (default_) {
    try {
      return int(key);
    } catch (e) {
      return default_;
    }
  };
};

function int(key) {
  const env = string(key);
  try {
    return parseInt(env);
  } catch (e) {
    throw new Error(`Expecting Int Env Var: ${key}`);
  }
}

function string(key) {
  const env = process.env[key];
  if (env) {
    return env;
  } else {
    throw new Error(`Missing Env Var: ${key}`);
  }
}
