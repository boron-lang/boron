use boron_source::prelude::SourceFileId;
use dashmap::DashMap;
use parking_lot::Mutex;
use petgraph::graph::{DiGraph, NodeIndex};

struct GraphWithMap {
  graph: Mutex<DiGraph<SourceFileId, ()>>,
  map: DashMap<SourceFileId, NodeIndex>,
}

impl GraphWithMap {
  fn new() -> Self {
    Self { graph: Mutex::new(DiGraph::new()), map: DashMap::new() }
  }

  fn ensure_node(&self, id: SourceFileId) -> NodeIndex {
    *self.map.entry(id).or_insert_with(|| self.graph.lock().add_node(id))
  }

  fn add_edge(&self, from: SourceFileId, to: SourceFileId) {
    let a = self.ensure_node(from);
    let b = self.ensure_node(to);
    self.graph.lock().add_edge(a, b, ());
  }
}

pub struct ModuleGraph {
  discovered: GraphWithMap,
}

impl ModuleGraph {
  pub fn new() -> Self {
    Self { discovered: GraphWithMap::new() }
  }

  pub fn add_discovered_relation(&self, from: SourceFileId, to: SourceFileId) {
    self.discovered.add_edge(from, to);
  }
}
