(() => {
  const link = location.href;
  const title = document.title || link;
  const md = document.title ? `[${title}](${link})` : `<${link}>`;
  const content = `<a href="${link}">${title}</a><br>${md}`;
  const wrapperStyle = "width: 300px; padding:5px; line-height:1.3em; font-size:120%; border:solid 1px black; background-color: white; position: fixed; top: 50%; left: calc(50% - 150px);"
  const html = `<div contenteditable style="${wrapperStyle}" autofocus id="kui_links">${content}</div>`;
  document.body.insertAdjacentHTML('beforeend', html);
})();
