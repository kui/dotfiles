// ==UserScript==
// @name         Web漫画アンテナ - ヘッダリンク追加
// @namespace    http://tampermonkey.net/
// @version      1.0
// @description  webcomics.jp のヘッダに任意のリンクを追加します
// @author       You
// @match        https://webcomics.jp/*
// @grant        none
// ==/UserScript==

(function() {
    'use strict';

    // ============================================
    // ここに追加したいリンクを書いてください
    // ============================================
    const LINKS = [
        { text: 'ホーム', url: 'https://webcomics.jp/' },
        // { text: '好きな名前', url: 'https://example.com' },
    ];
    // ============================================

    function addCustomLinks() {
        const naviUl = document.querySelector('#header .navi > ul');
        if (!naviUl) return;

        // 既に追加済みならスキップ
        if (naviUl.querySelector('.custom-header-link')) return;

        LINKS.forEach(link => {
            const li = document.createElement('li');
            const a = document.createElement('a');
            a.href = link.url;
            a.textContent = link.text;
            a.className = 'custom-header-link';
            a.style.color = '#fff';
            li.appendChild(a);
            naviUl.appendChild(li);
        });
    }

    // DOM読み込み後に実行
    if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', addCustomLinks);
    } else {
        addCustomLinks();
    }
})();
