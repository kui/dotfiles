((document) => {
  const link = location.href;
  const title = document.title || link;
  const faviconUrl =
    document.querySelector("link[rel*='icon']")?.href ??
    `${location.origin}/favicon.ico`;
  const methods = [
    ["Plain", textNode(`${title} ${link}`)],
    [
      "Markdown",
      textNode(document.title ? `[${title}](${link})` : `<${link}>`),
    ],
    ["HTML", textNode(`<a href="${link}">${escapeHtml(title)}</a>`)],
    [
      "DOM",
      buildElement("a", (a) => {
        a.href = link;
        a.textContent = title;
      }),
    ],
    [
      "Favicon",
      buildElement("span", (span) => {
        appendElement(span, "img", (img) => {
          img.src = faviconUrl;
          img.style = `width: 1em; display: inline;`;
        });
        span.appendChild(textNode(faviconUrl));
      }),
    ],
  ];

  function main() {
    const dialog = appendElement(document.body, "dialog", (dialog) => {
      for (const [labelText, node] of methods) {
        appendElement(dialog, "label", (label) => {
          label.style = "display: block;";
          appendElement(label, "input", (radio) => {
            radio.type = "radio";
            radio.name = "jp_kui_link_radio";
            radio.addEventListener("change", () => {
              if (radio.checked) copy(node);
            });
          });
          label.appendChild(textNode(`${labelText}\t`));
          label.appendChild(node);
        });
      }
      document.addEventListener("keydown", (event) => {
        if (event.key === "Enter") {
          dialog.close();
        }
      });
    });

    dialog.showModal();

    const first = dialog.querySelector("input");
    first.checked = true;
    first.focus();
  }

  function element(tag) {
    return document.createElement(tag);
  }
  function textNode(str) {
    return document.createTextNode(str);
  }
  function escapeHtml(s) {
    return buildElement("div", (d) => (d.textContent = s)).innerHTML;
  }
  function copy(node) {
    const selection = getSelection();
    const range = document.createRange();
    range.selectNodeContents(node);
    selection?.removeAllRanges();
    selection?.addRange(range);

    const commandResult = document.execCommand("copy");
    if (commandResult) {
      console.log("Copy Success", node);
    } else {
      console.log("Copy Failure", node);
    }
  }
  function buildElement(tag, func) {
    const e = element(tag);
    func(e);
    return e;
  }
  function appendElement(parent, tag, func) {
    return parent.appendChild(buildElement(tag, func));
  }

  main();
})(document);
