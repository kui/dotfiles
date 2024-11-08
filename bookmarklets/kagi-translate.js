(function () {
  const selectedText = window.getSelection().toString().trim();
  let param;
  if (selectedText) {
    param = `?text=${encodeURIComponent(selectedText)}`;
  } else {
    param = `/Japanese/${encodeURIComponent(window.location.href)}`;
  }
  window.open(`https://translate.kagi.com/${param}`, "_blank");
})()
