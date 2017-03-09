#!/usr/bin/env node

const { execFileSync } = require('child_process');

const padWindowId = (id) => {
  const missingZero = 8 - id.length + 1;
  return (new Array(missingZero)).join('0') + id;
};

const getFocusedWindow = () => {
  const stdout = execFileSync('xprop', ['-root', '_NET_ACTIVE_WINDOW'], { encoding: 'UTF-8' });
  const id = stdout.replace(/.*0x([a-z0-9]+)/, '$1').trim();
  return `0x${padWindowId(id)}`;
};

const getFocusedWorkspace = () => {
  const stdout = execFileSync('wmctrl', ['-d'], { encoding: 'UTF-8' });
  const workspaces = stdout.split('\n');
  const workspace = workspaces.find(workspace => {
    return /\*/.test(workspace);
  });
  return parseInt(workspace.split(' ')[0], 10);
};

const getWindows = (workspaceId) => {
  const stdout = execFileSync('wmctrl', ['-l'], { encoding: 'UTF-8' });
  const windows = stdout.split('\n');
  const pattern = new RegExp(` ${workspaceId} `);
  const workspaceWindows = windows.filter(window => {
    return pattern.test(window);
  });
  return workspaceWindows.map(window => {
    return window.replace(/(0x[a-z0-9]+).*/, '$1');
  });
};

const focusWindow = (window) => {
  execFileSync('wmctrl', ['-i', '-a', window]);
};

const getPreviousWindow = (windows, windowIndex) => {
  if (windowIndex > 0) {
    return windows[windowIndex - 1];
  }
  return windows[windows.length - 1];
};

const getNextWindow = (windows, windowIndex) => {
  if (windowIndex < windows.length - 1) {
    return windows[windowIndex + 1];
  }
  return windows[0];
};

const findWindow = (position) => {
  const workspace = getFocusedWorkspace();
  const windows = getWindows(workspace);

  if (windows.length < 2) {
    return null;
  }

  const window = getFocusedWindow();
  const windowIndex = windows.indexOf(window);

  if (position === 'p') {
    return getPreviousWindow(windows, windowIndex);
  }

  return getNextWindow(windows, windowIndex);
};

const main = (argv) => {
  const position = argv[2];
  const window = findWindow(position);
  if (window) {
    focusWindow(window);
  }
};

main(process.argv);
