((window, document) => {
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

  // extract imgage
  // twitter
  const query = "#permalink-overlay [data-element-context='platform_photo_card'] img";
  const imgs = Array.from($$(query))
      .map(i => i.src + ":orig");
  if (imgs.length !== 0) {
    q.posttype = "photo";
    q.content = imgs.map(encode).join(",");
  }

  const tumblr = "https://www.tumblr.com/widgets/share/tool?"
        +Object.entries(q).map(e => e.join("=")).join("&");
  const height = screen && screen.height || 600;
  window.open(tumblr, null, `height=${height},width=540`);
})(window, document);
