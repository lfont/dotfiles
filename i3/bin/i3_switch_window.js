#!/usr/bin/env node

const { execFileSync } = require('child_process');

const getTree = () => {
  const stdout = execFileSync('i3-msg', ['-t', 'get_tree'], { encoding: 'UTF-8' });
  return JSON.parse(stdout);
};

const sendCommands = (commands) => {
  const stdout = execFileSync('i3-msg', [commands.join(';')], { encoding: 'UTF-8' });
  return JSON.parse(stdout);
};

const getNodes = (tree) => {
  return tree.nodes.concat(tree.floating_nodes);
};

const filterNodes = (tree, predicate) => {
  return getNodes(tree).reduce((nodes, node) => {
    if (predicate(node)) {
      nodes.push(node);
    }
    return nodes.concat(filterNodes(node, predicate));
  }, []);
};

const getWindows = (tree, predicate) => {
  return filterNodes(tree, node =>
                     node.type === 'con'
                     && node.window
                     && predicate(node));
};

const getFocusedWindow = (tree) => {
  return getWindows(tree, node => node.focused)[0];
};

const getParent = (tree, child, predicate) => {
  const ancestor = filterNodes(tree, node =>
                               predicate(node)
                               && filterNodes(node, node => node === child).length)[0];
  let parent;
  if (ancestor) {
    let node = ancestor;
    while ((node = getParent(node, child, predicate))) {
      parent = node;
    }
  }
  return parent || ancestor;
};

const getWorkspace = (tree, window) => getParent(tree, window, node => node.type === 'workspace');

const isFloatingWindow = (window) => window.floating === 'user_on';

const getContainer = (workspace, window) => {
  if (isFloatingWindow(window)) {
    return workspace;
  }
  return getParent(workspace, window, node => node.type === 'con');
};

const getWindowsPartition = (workspace) => {
  const windows = getWindows(workspace, _ => true);
  const partition = windows.reduce((partition, window) => {
    if (isFloatingWindow(window)) {
      partition.floating.push(window);
    } else {
      partition.tilling.push(window);
    }
    return partition;
  }, { floating: [], tilling: [] });
  partition.floating = partition.floating.sort((a, b) => {
    if (a.rect.x < b.rect.x) {
      return -1;
    }
    if (a.rect.x > b.rect.x) {
      return 1;
    }
    return 0;
  });
  return partition;
};

const getPosition = (windowsPartition, window) => {
  const windows = isFloatingWindow(window)
        ? windowsPartition.floating
        : windowsPartition.tilling;
  const index = windows.indexOf(window);
  let position = 0;
  if (index === 0) {
    position += 2;
  }
  if (index === windows.length - 1) {
    position += 4;
  }
  console.log('*** position:', windows.length, index, position);
  return position;
};

const translateDirection = (container, direction) => {
  if (container.type === 'workspace' || ['tabbed', 'splith'].indexOf(container.layout) > -1) {
    return direction === 'previous' ? 'focus left' : 'focus right';
  }
  return direction === 'previous' ? 'focus up' : 'focus down';
};

const computeTranslateDirections = (container, windows, direction) => {
  // On a multi monitor setup trying to go a step further on the first/last
  // window put focus on the next monitor instead of focusing the first/last
  // window on the same monitor. We have to manually move to the opposite
  // direction.
  const translatedDirection = translateDirection(container, direction);
  return windows
    .map(_ => translatedDirection)
    .slice(1);
};

const computeToggleModeDirections = (container, windows, direction) => {
  // Cycling should be predictable, so after a toggle
  // we should be on the first or last window of a container
  // not the previously focused one.
  const directions = [
    'focus mode_toggle',
    direction === 'previous'
      ? `[con_id=${windows[windows.length - 1].id}] focus`
      : `[con_id=${windows[0].id}] focus`
  ];
  return directions.concat(
    computeTranslateDirections(container, windows, direction)
  );
};

const computeReverseDirections = (workspace, windowsPartition, window, direction) => {
  if (!windowsPartition.floating.length) {
    const container = getContainer(workspace, window);
    console.log('*** reverse:', 't -> t');
    return computeTranslateDirections(container, windowsPartition.tilling, direction);
  }
  if (!windowsPartition.tilling.length) {
    const container = getContainer(workspace, window);
    console.log('*** reverse:', 'f -> f');
    return computeTranslateDirections(container, windowsPartition.floating, direction);
  }
  if (isFloatingWindow(window)) {
    const container = getContainer(workspace, windowsPartition.tilling[0]);
    console.log('*** reverse:', 'f -> t');
    return computeToggleModeDirections(container, windowsPartition.tilling, direction);
  }
  const container = getContainer(workspace, windowsPartition.floating[0]);
  console.log('*** reverse:', 't -> f');
  return computeToggleModeDirections(container, windowsPartition.floating, direction);
};

const computeDirections = (tree, window, direction) => {
  // Focus does not raise a floating window.
  // We use direction to focus and raise.
  const workspace = getWorkspace(tree, window);
  const windowsPartition = getWindowsPartition(workspace);
  const position = getPosition(windowsPartition, window);
  if (direction === 'previous' && (position === 2 || position === 6)) {
    return computeReverseDirections(workspace, windowsPartition, window, 'next');
  }
  if (direction === 'next' && (position === 4 || position === 6)) {
    return computeReverseDirections(workspace, windowsPartition, window, 'previous');
  }
  const container = getContainer(workspace, window);
  return [translateDirection(container, direction)];
};

const main = (argv) => {
  // FIXME: Does not work with nested containers.
  const direction = argv[2];
  const tree = getTree();
  const window = getFocusedWindow(tree);
  if (window) {
    const commands = computeDirections(tree, window, direction);
    console.log('*** commands:', commands);
    const status = sendCommands(commands);
    console.log('*** status:', status);
  }
};

main(process.argv);
