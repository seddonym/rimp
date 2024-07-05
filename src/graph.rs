#![allow(dead_code)]

use petgraph::stable_graph::NodeIndex;
use petgraph::stable_graph::StableGraph;
use std::collections::{HashMap, HashSet};

// Delimiter for Python modules.
const DELIMITER: char = '.';

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Module {
    name: String,
}

impl Module {
    pub fn new(name: String) -> Module {
        Module { name }
    }

    // Returns whether the module is a root-level package.
    pub fn is_root(&self) -> bool {
        !self.name.contains(DELIMITER)
    }

    // Create a Module that is the parent of the passed Module.
    //
    // Panics if the child is a root Module.
    pub fn new_parent(child: &Module) -> Module {
        let parent_name = match child.name.rsplit_once(DELIMITER) {
            Some((base, _)) => base.to_string(),
            None => panic!("{} is a root level package", child.name),
        };

        Module::new(parent_name)
    }
}
struct Graph<'a> {
    pub modules: HashSet<Module>,
    module_indices: HashMap<NodeIndex, Module>,
    hierarchy: StableGraph<&'a Module, ()>,
}

impl<'a> Graph<'a> {
    pub fn new() -> Graph<'a> {
        Graph {
            modules: HashSet::new(),
            module_indices: HashMap::new(),
            hierarchy: StableGraph::new(),
        }
    }

    pub fn add_module(&mut self, module: &'a Module) {
        let index = self.hierarchy.add_node(module);
        if !module.is_root() {
            //let parent = Module::new_parent(&module);

            //self.hierarchy.add_edge(&parent, &module, ());
        }
        self.module_indices.insert(index, module.clone());
        self.modules.insert(module.clone());
    }

    pub fn find_children(&self, module: &'a Module) -> HashSet<&'a Module> {
        HashSet::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn modules_when_empty() {
        let graph = Graph::new();

        assert_eq!(graph.modules, HashSet::new());
    }

    #[test]
    fn module_is_value_object() {
        assert_eq!(
            Module::new("mypackage".to_string()),
            Module::new("mypackage".to_string())
        );
    }

    #[test]
    fn add_module() {
        let mut graph = Graph::new();
        let mypackage = Module::new("mypackage".to_string());

        graph.add_module(&mypackage);

        assert_eq!(graph.modules, HashSet::from([mypackage]));
    }

    #[test]
    fn add_modules() {
        let mut graph = Graph::new();
        let mypackage = Module::new("mypackage".to_string());
        let mypackage_foo = Module::new("mypackage.foo".to_string());

        graph.add_module(&mypackage);
        graph.add_module(&mypackage_foo);

        assert_eq!(graph.modules, HashSet::from([mypackage, mypackage_foo]));
    }

    #[test]
    fn find_children_no_results() {
        let mut graph = Graph::new();
        let mypackage = Module::new("mypackage".to_string());
        let mypackage_foo = Module::new("mypackage.foo".to_string());

        graph.add_module(&mypackage);
        graph.add_module(&mypackage_foo);

        assert_eq!(graph.find_children(&mypackage_foo), HashSet::new());
    }

    #[test]
    #[should_panic(expected = "rootpackage is a root level package")]
    fn new_parent_root_module() {
        let root = Module::new("rootpackage".to_string());

        Module::new_parent(&root);
    }

    #[test]
    fn is_root_true() {
        let root = Module::new("rootpackage".to_string());

        assert!(root.is_root());
    }

    #[test]
    fn is_root_false() {
        let non_root = Module::new("rootpackage.blue".to_string());

        assert_eq!(non_root.is_root(), false);
    }

    #[test]
    #[should_panic]
    fn find_children_one_result() {
        let mut graph = Graph::new();
        let mypackage = Module::new("mypackage".to_string());
        let mypackage_foo = Module::new("mypackage.foo".to_string());
        let mypackage_bar = Module::new("mypackage.bar".to_string());

        graph.add_module(&mypackage);
        graph.add_module(&mypackage_foo);
        graph.add_module(&mypackage_bar);

        assert_eq!(
            graph.find_children(&mypackage),
            HashSet::from([&mypackage_foo, &mypackage_bar])
        );
    }

    // #[test]
    // fn tutorial() {
    //     use petgraph::graph::Graph;
    //     use petgraph::graphmap::DiGraphMap;

    //     let mut graph = DiGraphMap::<&Module, _>::new();
    //     let foo = Module::new("foo".to_string());
    //     graph.add_node(&foo);
    //     let blue = Module::new("foo.blue".to_string());
    //     graph.add_node(&blue);
    //     let green = Module::new("foo.green".to_string());
    //     graph.add_node(&green);

    //     graph.add_edge(&foo, &blue, 1);
    //     graph.add_edge(&foo, &green, 1);

    //     let children: Vec<&Module> = graph
    //         .neighbors_directed(&foo, petgraph::Direction::Outgoing)
    //         .collect();
    //     assert_eq!(children, vec![&blue, &green]);
    // }

    #[test]
    fn find_children_multiple_results() {
        let mut graph = Graph::new();
        let mypackage = Module::new("mypackage".to_string());
        let mypackage_foo = Module::new("mypackage.foo".to_string());
        let mypackage_bar = Module::new("mypackage.bar".to_string());

        graph.add_module(&mypackage);
        graph.add_module(&mypackage_foo);
        graph.add_module(&mypackage_bar);

        assert_eq!(
            graph.find_children(&mypackage),
            HashSet::from([&mypackage_foo, &mypackage_bar])
        );
    }
}
