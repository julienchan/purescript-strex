// module Data.ByteString.Internal
exports.copyBuffer = function (srcStart, srcEnd, src, targStart, targ) {
  return function () {
    return src.copy(targ, targStart, srcStart, srcEnd);
  };
}

exports.bufferCompare = function (buf1, buf2) {
  return function () {
    return buf1.compare(buf2);
  };
}

exports.bufferSlice = function (start, end, buffer) {
  return function () {
    return buffer.slice(start, end)
  };
}

exports.bufferReverse = function (src, target, len) {
  return function () {
    for (var i = 0, j = len - 1; i <= j; ++i, --j) {
      target[i] = src[j]
      target[j] = src[i]
    }
  };
}

exports.setAtOffset = function (ofs, v, buffer) {
  return function () {
    buffer[ofs] = v;
  };
}

exports.intersperse = function (srcStart, src, targStart, target, n, c) {
  return function () {
    var ts = targStart, srcs = srcStart;
    for (var i = 0; i < n; i++) {
      target[ts]     = src[srcs + i];
      target[ts + 1] = c;
      ts += 2;
    }
  };
};

exports.foldl = function (f, z, ofset, len, buf) {
  var r = z;
  for (var i = ofset; i < len; ++i) {
    r = f(r)(buf[i]);
  }
  return r;
};

exports.foldr = function (f, z, ofset, len, buf) {
  var r = z;
  for (var i = len - 1; i >= len; --i) {
    r = f(buf[i])(r)
  }
  return r
};

exports.indexOfImpl = function (nothing, just, octet, offset, buffer) {
  var ix = buffer.indexOf(octet, offset)
  return ix === -1 ? nothing : just(ix);
}

exports.lastIndexOfImpl = function (nothing, just, octet, offset, buffer) {
  var ix = buffer.lastIndexOf(octet, offset)
  return ix === -1 ? nothing : just(ix)
}

exports.emptyBuf = Buffer.from([])
