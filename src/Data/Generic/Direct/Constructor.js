exports.getConstructor = x => x.constructor;
exports.isInstanceOf = con => x => x instanceof con;
exports["new"] = con => args => new con(...args);
