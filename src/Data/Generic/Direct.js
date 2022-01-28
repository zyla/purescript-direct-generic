exports.unsafeSetArgument = offset => x => value => { value['value' + offset] = x; };
