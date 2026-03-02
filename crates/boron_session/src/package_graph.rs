use crate::dependency::Dependency;
use petgraph::Direction;
use petgraph::algo::toposort;
use petgraph::graph::{DiGraph, NodeIndex};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Default)]
pub struct PackageGraph {
  graph: DiGraph<String, ()>,
  node_indices: HashMap<String, NodeIndex>,
}

impl PackageGraph {
  pub fn new() -> Self {
    Self::default()
  }

  pub fn add_package(&mut self, name: &str) -> NodeIndex {
    if let Some(&idx) = self.node_indices.get(name) {
      return idx;
    }
    let idx = self.graph.add_node(name.to_owned());
    self.node_indices.insert(name.to_owned(), idx);
    idx
  }

  /// `from` depends on `to`, so `to` must be compiled before `from`.
  pub fn add_dependency(&mut self, from: &str, to: &str) {
    let to_idx = self.add_package(to);
    let from_idx = self.add_package(from);
    self.graph.add_edge(to_idx, from_idx, ());
  }

  pub fn from_dependencies(deps: &[Dependency]) -> Self {
    let mut g = Self::new();
    for dep in deps {
      g.add_package(&dep.name);
    }
    for dep in deps {
      for needed in &dep.depends_on {
        g.add_dependency(&dep.name, needed);
      }
    }
    g
  }

  pub fn compilation_order<'a>(
    &self,
    deps: &'a [Dependency],
  ) -> Result<Vec<&'a Dependency>, PackageCycleError> {
    match toposort(&self.graph, None) {
      Ok(order) => {
        let sorted = order
          .into_iter()
          .filter_map(|idx| {
            let name = &self.graph[idx];
            deps.iter().find(|d| &d.name == name)
          })
          .collect();
        Ok(sorted)
      }
      Err(cycle) => Err(PackageCycleError { cycle: self.find_cycle(cycle.node_id()) }),
    }
  }

  pub fn package_count(&self) -> usize {
    self.graph.node_count()
  }

  pub fn is_empty(&self) -> bool {
    self.graph.node_count() == 0
  }

  fn find_cycle(&self, start: NodeIndex) -> Vec<String> {
    let mut path: Vec<NodeIndex> = Vec::new();
    let mut on_stack: HashSet<NodeIndex> = HashSet::new();
    let mut visited: HashSet<NodeIndex> = HashSet::new();

    if Self::dfs_find_cycle(&self.graph, start, &mut path, &mut on_stack, &mut visited) {
      path.iter().map(|&idx| self.graph[idx].clone()).collect()
    } else {
      vec![self.graph[start].clone()]
    }
  }

  fn dfs_find_cycle(
    graph: &DiGraph<String, ()>,
    node: NodeIndex,
    path: &mut Vec<NodeIndex>,
    on_stack: &mut HashSet<NodeIndex>,
    visited: &mut HashSet<NodeIndex>,
  ) -> bool {
    if on_stack.contains(&node) {
      let cycle_start = path.iter().position(|&n| n == node).unwrap_or(0);
      path.drain(..cycle_start);
      path.push(node);
      return true;
    }
    if visited.contains(&node) {
      return false;
    }

    path.push(node);
    on_stack.insert(node);

    for neighbor in graph.neighbors_directed(node, Direction::Outgoing) {
      if Self::dfs_find_cycle(graph, neighbor, path, on_stack, visited) {
        return true;
      }
    }

    path.pop();
    on_stack.remove(&node);
    visited.insert(node);
    false
  }
}

#[derive(Debug)]
pub struct PackageCycleError {
  pub cycle: Vec<String>,
}

impl std::fmt::Display for PackageCycleError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "cyclic package dependency: {}", self.cycle.join(" → "))
  }
}

impl std::error::Error for PackageCycleError {}
