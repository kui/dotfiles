((window, document) => {
  const encode = window.encodeURIComponent;
  const $ = document.querySelector.bind(document);
  const $$ = document.querySelectorAll.bind(document);
  let t;
  let url = (t=$("link[rel=canonical]")) && t.href;
  url = url || ((t=$('meta[property="og:url"],meta[property="twitter:url"]')) && t.content);
  url = url || location.href;
  let title = document.title;

  let q = {
    url: encode(url),
    caption: encode(`<a href="${url}">${title}</a>`),
  };

  // extract imgage

  // twitter images
  let query = "#permalink-overlay [data-element-context='platform_photo_card'] img";
  let imgs = Array.from($$(query))
      .map(i => i.src + ":orig");
  if (imgs.length !== 0) {
    q.posttype = "photo";
    q.content = imgs.map(encode).join(",");
  }

  window.open('https://www.tumblr.com/widgets/share/tool?'+Object.entries(q).map(e => e.join("=")).join("&"),
              '_blank');
})(window, document);
