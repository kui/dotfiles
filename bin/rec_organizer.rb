#!/usr/bin/env ruby
# -*- coding: utf-8 -*-
$KCODE = 'u'

USAGE = <<_USAGE

usage: $0 <base_dir>
<base_dir> 直下にある Chinatu で録画したファイル *.\{m2,}ts を
番組名のディレクトリでまとめる

_USAGE

require 'fileutils'
require 'optparse'

def main base_dir
  @move = false

  op = OptionParser.new USAGE do |opt|
    opt.on('-r', '--run', 'move files'){|b| @move = b}
  end
  op.parse! ARGV

  if ARGV.length != 1
    puts op.help
    exit false
  end

  Dir[File.join(base_dir, '*.{m2ts,mp4}')].each do |ts|
    dest_dir_name = generate_dir_name ts
    dest_dir = File.join base_dir, dest_dir_name
    puts "\t=> #{dest_dir}"


    if not FileTest.directory? dest_dir
      FileUtils.mkdir_p dest_dir, :verbose => true
    end

    if @move
      FileUtils.move ts, dest_dir, :verbose => true
    end

    puts '###'
  end
end

def generate_dir_name ts
  puts ts

  # ディレクトリ部分の削除
  ts = File.basename ts

  # 拡張子の削除
  ts.sub!(/\A(.*).(?:(?:m2)ts|mp4)\Z/, '\1')

  # Chinatu が付与するメタデータ削除
  ts.sub!(/\A(?:\[.*?\])*/, '')

  # 番組名に付与されるメタデータ削除
  ts.sub!(/\A(?:【.】)*/, '')
  ts.sub!(/(?:【.】)*\Z/, '')

  # 回数番号を削除
  num = "[０-９0-9一二三四五六七八九十壱弐参拾〇IⅡⅢⅣⅤⅥⅦⅧⅨⅩⅪⅫVX]+"
  ts.sub!(/[♯＃\#]#{num}/, '')
  ts.sub!(/第?#{num}話/, '')
  ts.sub!(/Episode ?#{num}/, '')
  ts.sub!(/特別編/, '')

  # 副題削除
  ts.sub!(/「.*?」/, '')

  # TsSplitter の接尾辞削除
  ts.sub!(/_HD(?:-\d+)\Z/, '')

  # 先頭・末尾の空白削除
  ts.sub!(/\A[\s　]*/, '')
  ts.sub!(/[\s　]*\Z/, '')

  ts
end

main ARGV
