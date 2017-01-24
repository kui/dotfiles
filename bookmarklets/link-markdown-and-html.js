(() => {
  const link = location.href;
  const title = document.title || link;
  const md = document.title ? `[${title}](${link})` : `<${link}>`;
  const content = `<a href="${link}">${title}</a><br>${md}`;
  const wrapperStyle = "padding:5px; line-height:1.3em; font-size:120%; border:solid 1px black;"
  const html = `<div contenteditable style="${wrapperStyle}" autofocus>${content}</div>`;
  window.open(`data:text/html;charset=utf-8,${html}`,'_blank')
})();
