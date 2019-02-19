// Write

const os = require('os');
const path = require('path');
const fs = require('fs');
const complexModConfig = require('./kui-karabiner');

const targetFile = path.join(os.homedir(), '.config/karabiner/karabiner.json');
const targetDir = path.dirname(targetFile);

const config = require(targetFile);

const defaultProfile = config.profiles.find(p => p.name.toLowerCase() === 'default profile');
if (!defaultProfile) {
  console.error('ERROR: "Default profile" not found');
  return 1;
}

defaultProfile.rules = complexModConfig.rules;

console.log('mkdir', targetDir)
fs.mkdirSync(targetDir, { recursive: true });

console.log('create backup')
fs.copyFileSync(targetFile, `${targetFile}.bk`);

console.log('write into', targetFile)
fs.writeFileSync(targetFile, JSON.stringify(config, 0, 2));
