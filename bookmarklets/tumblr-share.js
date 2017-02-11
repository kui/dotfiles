((window, document) => {
  function uniq(array) {
    return Array.from(new Set(array));
  }

  const encode = window.encodeURIComponent;
  const $ = document.querySelector.bind(document);
  const $$ = document.querySelectorAll.bind(document);
  const url = ((t=$("link[rel=canonical]")) && t.href)
        || ((t=$('meta[property="og:url"],meta[property="twitter:url"]')) && t.content)
        || location.href;
  const title = document.title;

  const q = {
    url: encode(url),
    caption: encode(`<a href="${url}">${title}</a>`),
  };

  const contentExtractors = [
    () => {
      const selection = window.getSelection();
      if (selection.rangeCount === 0) return;

      let imgs = [];
      for (const e of selection.getRangeAt(0).commonAncestorContainer.querySelectorAll("*")) {
        if (!selection.containsNode(e, true)) continue;
        if (e.tagName === "IMG" && e.src) {
          imgs.push(e.src);
        }
        const style = window.getComputedStyle(e);
        const urlMatch = /^url\(["']?([^"']+)["']?\)$/.exec(style.backgroundImage);
        if (urlMatch) {
          imgs.push(urlMatch[1]);
        }
      }
      if (imgs.length === 0) return;
      imgs = uniq(imgs);
      console.log("imgs", imgs);

      q.posttype = "photo";
      q.content = imgs.map(encode).join(",");
    },
    () => {
      const query = "#permalink-overlay [data-element-context='platform_photo_card'] img";
      let imgs = Array.from($$(query)).map(i => i.src + ":orig");
      if (imgs.length === 0) return;
      imgs = uniq(imgs);
      console.log("imgs", imgs);

      q.posttype = "photo";
      q.content = uniq(imgs).map(encode).join(",");
    },
  ];
  contentExtractors.some((e) => {
    e();
    return q.content != null;
  });

  const tumblr = "https://www.tumblr.com/widgets/share/tool?"
        +Object.entries(q).map(e => e.join("=")).join("&");
  const screen = window.screen;
  const height = screen && screen.height || 600;
  window.open(tumblr, null, `height=${height},width=540`);

  const tmpWin = window.open();
  if (tmpWin) tmpWin.close();
})(window, document);
