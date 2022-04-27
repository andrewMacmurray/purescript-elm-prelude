exports.unsafe_ = unsafe;

exports.optional_ = function (key) {
  return function (default_) {
    const env = process.env[key];
    return env || default_;
  };
};

exports.unsafeInt_ = (key) => {
  const env = unsafe(key);
  try {
    return parseInt(env);
  } catch (e) {
    throw new Error(`Expecting Int Env Var: ${key}`);
  }
};

function unsafe(key) {
  const env = process.env[key];
  if (env) {
    return env;
  } else {
    throw new Error(`Missing Env Var: ${key}`);
  }
}
