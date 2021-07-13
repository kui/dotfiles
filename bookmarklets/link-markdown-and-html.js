(() => {
  const link = location.href;
  const title = document.title || link;
  const faviconUrl = `https://s2.googleusercontent.com/s2/favicons?domain_url=${encodeURIComponent(link)}`;

  function escapeHtml(s) {
    const d = document.createElement('div');
    d.textContent = s;
    return d.innerHTML;
  }
  function hasInParents(e, fn) {
    if (fn(e)) return true;
    if (e === document.body) return false;
    return hasInParents(e.parentElement, fn);
  }
  function copy(s) {
    const textarea = document.createElement("textarea");
    textarea.textContent = s;
    document.body.appendChild(textarea);

    textarea.select();

    console.log('copy %s', s);
    const r = document.execCommand('copy');
    console.log('result: %s', r);

    document.body.removeChild(textarea);
  }

  const wrapper = document.createElement('div');
  wrapper.id = 'jp_kui_wrapper';
  wrapper.style = 'top: 0;left: 0;width: 100%;height: 100%;position: fixed;z-index: 2147483647 ;background-color: rgba(0, 0, 0, .5);';

  const container = document.createElement('div');
  container.style = 'width: 500px;padding: 5px;line-height: 24px;font-size: 16px;color: black;background-color: white;border: solid 1px black;position: fixed;top: 20%;left: calc(50% - 250px);';
  wrapper.appendChild(container);

  function appendPart(f) {
    container.appendChild(f());
  }
  function appendBr() {
    appendPart(() => document.createElement('br'));
  }
  function appendText(s) {
    appendPart(() => document.createTextNode(s));
  }
  function appendChoice(labelText, value) {
    appendPart(() => {
      const label = document.createElement('label');
      label.style = 'display: block;';
      const radio = document.createElement('input');
      radio.type = 'radio';
      radio.value = value;
      radio.autofocus = true;
      radio.name = 'jp_kui_copy_target';
      label.appendChild(radio);
      label.appendChild(document.createTextNode(` ${labelText}`));

      radio.addEventListener('change', () => {
        for (const e of document.getElementsByName('jp_kui_copy_target')) {
          if (e.checked) {
            e.style.backgroundColor = 'yellow';
          } else {
            e.style.backgroundColor = null;
          }
        }
      });

      return label;
    });
  }

  appendPart(() => {
    const part = document.createElement('a');
    part.href = link;
    const faviconImg = document.createElement('img');
    faviconImg.src = faviconUrl;
    part.appendChild(faviconImg);
    part.appendChild(document.createTextNode(` ${title}`));
    return part;
  });

  appendBr();

  appendText('Copy title + link as:');

  appendBr();

  appendChoice(
    'Markdown',
    document.title ? `[${title}](${link})` : `<${link}>`
  );

  appendChoice(
    'Markdown + Icon',
    `[![](${faviconUrl}) ${title}](${link})`
  );

  appendChoice(
    'HTML Source',
    `<a href="${link}">${escapeHtml(title)}</a>`
  );

  appendChoice(
    'HTML Source (with Icon)',
    `<a href="${link}"><img src="${faviconUrl}"> ${escapeHtml(title)}</a>`
  );

  function copyAndClose() {
    for (const e of document.getElementsByName('jp_kui_copy_target')) {
      if (!e.checked) continue;
      copy(e.value);
      break;
    }
    document.body.removeChild(wrapper);
  }

  appendPart(() => {
    const button = document.createElement('button');
    button.textContent = 'Copy';
    button.addEventListener('click', (event) => {
      event.stopPropagation();
      copyAndClose();
    });
    return button;
  });

  wrapper.addEventListener('click', (event) => {
    if (event.target === wrapper) document.body.removeChild(wrapper);
  });
  wrapper.addEventListener('keydown', (event) => {
    if (event.key !== 'Enter') return;
    event.stopPropagation();
    copyAndClose();
  });
  document.body.appendChild(wrapper);
})();
