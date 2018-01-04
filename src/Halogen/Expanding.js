exports.computedStyle = function(element) {
  return function() {
    return getComputedStyle(element, null).cssText;
  };
};

exports.fixStyle = function(element) {
  return function() {
    element.style.width = '';
    element.style.position = '';
  }
}
