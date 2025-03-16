(function () {
  const selection = window.getSelection().toString();
  const prefix = selection.length === 0 ? '' : `"${selection}" -`;
  const tweet = `${prefix} ${document.title} ${location.href}`;
  window.open('https://twitter.com/intent/tweet?text=' + encodeURIComponent(tweet), '_blank');
})();
