((window, document) => {
  function uniq(array) {
    // Donot use Array.from() because some site overwrite it.
    const a = [];
    for (const e of new Set(array)) a.push(e);
    return a;
  }

  const $ = document.querySelector.bind(document);
  const $$ = document.querySelectorAll.bind(document);
  const url = ((t=$("link[rel=canonical]")) && t.href)
        || ((t=$('meta[property="og:url"],meta[property="twitter:url"]')) && t.content)
        || location.href;
  const title = document.title;

  const q = {
    url: url,
    caption: `<a href="${url}">${title}</a>`,
  };

  const contentExtractors = [
    () => { // selected images
      const selection = window.getSelection();
      if (selection.rangeCount === 0) return;

      const ancestor = selection.getRangeAt(0).commonAncestorContainer;
      if (!ancestor.querySelectorAll) return;

      let imgs = [];
      for (const e of ancestor.querySelectorAll("*")) {
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
      q.content = imgs.join(",");
    },
    () => { // twitter images
      let imgs = Array
          .from(document.querySelectorAll("img"))
          .filter(i => i.src.match(/\/media\//))
          .map(i => i.src.replace(/&name=.*?$/, ""));
      if (imgs.length === 0) return;
      imgs = uniq(imgs);
      console.log("imgs", imgs);

      q.url = q.url.replace(/\?.*$/, "");
      q.posttype = "photo";
      q.content = imgs.join(",");
    },
    () => { // pixiv
      let imgs = Array.from(document.querySelectorAll('img.original-image')).map(i => i.dataset["src"]);
      if (imgs.length === 0) return;
      imgs = uniq(imgs);
      console.log("imgs", imgs);

      q.posttype = "photo";
      q.content = imgs.join(",");
    }
  ];
  contentExtractors.some((e) => {
    e();
    return q.content != null;
  });

  console.log(q);

  const tumblr = "https://www.tumblr.com/widgets/share/tool?"
        +Object.entries(q).map(e => e.map(window.encodeURIComponent).join("=")).join("&");
  const screen = window.screen;
  const height = screen && screen.height || 600;
  window.open(tumblr, null, `height=${height},width=540`);

  const tmpWin = window.open();
  if (tmpWin) tmpWin.close();
})(window, document);
