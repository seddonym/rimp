#![allow(dead_code)]

use bimap::BiMap;
use petgraph::stable_graph::{NodeIndex, StableGraph};
use petgraph::visit::{Bfs, Walker};
use std::collections::HashSet;

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
#[derive(Default)]
struct Graph {
    // Bidirectional lookup between Module and NodeIndex.
    hierarchy_module_indices: BiMap<Module, NodeIndex>,
    hierarchy: StableGraph<Module, ()>,
    imports_module_indices: BiMap<Module, NodeIndex>,
    imports: StableGraph<Module, ()>,
}

impl Graph {
    pub fn add_module(&mut self, module: Module) {
        let module_index = self.hierarchy.add_node(module.clone());
        self.hierarchy_module_indices
            .insert(module.clone(), module_index);

        // Add to the hierarchy from the module's parent, if it has one.
        if !module.is_root() {
            let parent = Module::new_parent(&module);

            // If the parent isn't already in the graph, add it.
            let parent_index = match self.hierarchy_module_indices.get_by_left(&parent) {
                Some(index) => index,
                None => {
                    self.add_module(parent.clone());
                    self.hierarchy_module_indices.get_by_left(&parent).unwrap()
                }
            };

            self.hierarchy.add_edge(*parent_index, module_index, ());
        }
    }

    pub fn get_modules(&self) -> HashSet<&Module> {
        self.hierarchy_module_indices.left_values().collect()
    }

    pub fn find_children(&self, module: &Module) -> HashSet<&Module> {
        let module_index = self.hierarchy_module_indices.get_by_left(module).unwrap();
        self.hierarchy
            .neighbors(*module_index)
            .map(|index| self.hierarchy_module_indices.get_by_right(&index).unwrap())
            .collect()
    }

    pub fn find_descendants(&self, module: &Module) -> HashSet<&Module> {
        let module_index = self.hierarchy_module_indices.get_by_left(module).unwrap();
        Bfs::new(&self.hierarchy, *module_index)
            .iter(&self.hierarchy)
            .filter(|index| index != module_index) // Don't include the supplied module.
            .map(|index| self.hierarchy_module_indices.get_by_right(&index).unwrap())
            .collect()
    }

    #[allow(unused)]
    pub fn add_import(&mut self, importer: &Module, imported: &Module) {
        let importer_index = self.imports.add_node(importer.clone());
        self.imports_module_indices
            .insert(importer.clone(), importer_index);
        let imported_index = self.imports.add_node(imported.clone());
        self.imports_module_indices
            .insert(imported.clone(), imported_index);

        self.imports.add_edge(importer_index, imported_index, ());
    }

    #[allow(unused)]
    pub fn direct_import_exists(&self, importer: &Module, imported: &Module) -> bool {
        let importer_index = *self.imports_module_indices.get_by_left(importer).unwrap();
        let imported_index = *self.imports_module_indices.get_by_left(imported).unwrap();

        self.imports.contains_edge(importer_index, imported_index)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn modules_when_empty() {
        let graph = Graph::default();

        assert_eq!(graph.get_modules(), HashSet::new());
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
        let mypackage = Module::new("mypackage".to_string());
        let mut graph = Graph::default();
        graph.add_module(mypackage.clone());

        let result = graph.get_modules();

        assert_eq!(result, HashSet::from([&mypackage]));
    }

    #[test]
    fn add_modules() {
        let mut graph = Graph::default();
        let mypackage = Module::new("mypackage".to_string());
        let mypackage_foo = Module::new("mypackage.foo".to_string());
        graph.add_module(mypackage.clone());
        graph.add_module(mypackage_foo.clone());

        let result = graph.get_modules();

        assert_eq!(result, HashSet::from([&mypackage, &mypackage_foo]));
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
    fn find_children_no_results() {
        let mut graph = Graph::default();
        let mypackage = Module::new("mypackage".to_string());
        let mypackage_foo = Module::new("mypackage.foo".to_string());

        graph.add_module(mypackage.clone());
        graph.add_module(mypackage_foo.clone());

        assert_eq!(graph.find_children(&mypackage_foo), HashSet::new());
    }

    #[test]
    fn find_children_one_result() {
        let mut graph = Graph::default();
        let mypackage = Module::new("mypackage".to_string());
        let mypackage_foo = Module::new("mypackage.foo".to_string());
        let mypackage_bar = Module::new("mypackage.bar".to_string());

        graph.add_module(mypackage.clone());
        graph.add_module(mypackage_foo.clone());
        graph.add_module(mypackage_bar.clone());

        assert_eq!(
            graph.find_children(&mypackage),
            HashSet::from([&mypackage_foo, &mypackage_bar])
        );
    }

    #[test]
    fn find_children_multiple_results() {
        let mut graph = Graph::default();
        let mypackage = Module::new("mypackage".to_string());
        let mypackage_foo = Module::new("mypackage.foo".to_string());
        let mypackage_bar = Module::new("mypackage.bar".to_string());

        graph.add_module(mypackage.clone());
        graph.add_module(mypackage_foo.clone());
        graph.add_module(mypackage_bar.clone());

        assert_eq!(
            graph.find_children(&mypackage),
            HashSet::from([&mypackage_foo, &mypackage_bar])
        );
    }

    #[test]
    fn find_children_works_when_adding_orphans() {
        let mut graph = Graph::default();
        // Note: mypackage is not in the graph.
        let mypackage_foo = Module::new("mypackage.foo".to_string());
        let mypackage_bar = Module::new("mypackage.bar".to_string());

        graph.add_module(mypackage_foo.clone());
        graph.add_module(mypackage_bar.clone());

        assert_eq!(
            graph.find_children(&Module::new("mypackage".to_string())),
            HashSet::from([&mypackage_foo, &mypackage_bar])
        );
    }

    #[test]
    fn find_descendants_no_results() {
        let mut graph = Graph::default();
        let mypackage = Module::new("mypackage".to_string());
        let mypackage_foo = Module::new("mypackage.foo".to_string());
        let mypackage_bar = Module::new("mypackage.bar".to_string());
        let mypackage_foo_alpha = Module::new("mypackage.foo.alpha".to_string());
        let mypackage_foo_alpha_blue = Module::new("mypackage.foo.alpha.blue".to_string());
        let mypackage_foo_alpha_green = Module::new("mypackage.foo.alpha.green".to_string());
        let mypackage_foo_beta = Module::new("mypackage.foo.beta".to_string());

        graph.add_module(mypackage.clone());
        graph.add_module(mypackage_foo.clone());
        graph.add_module(mypackage_bar.clone());
        graph.add_module(mypackage_foo_alpha.clone());
        graph.add_module(mypackage_foo_alpha_blue.clone());
        graph.add_module(mypackage_foo_alpha_green.clone());
        graph.add_module(mypackage_foo_beta.clone());

        assert_eq!(graph.find_descendants(&mypackage_bar), HashSet::new());
    }

    #[test]
    fn find_descendants_multiple_results() {
        let mut graph = Graph::default();
        let mypackage = Module::new("mypackage".to_string());
        let mypackage_foo = Module::new("mypackage.foo".to_string());
        let mypackage_bar = Module::new("mypackage.bar".to_string());
        let mypackage_foo_alpha = Module::new("mypackage.foo.alpha".to_string());
        let mypackage_foo_alpha_blue = Module::new("mypackage.foo.alpha.blue".to_string());
        let mypackage_foo_alpha_green = Module::new("mypackage.foo.alpha.green".to_string());
        let mypackage_foo_beta = Module::new("mypackage.foo.beta".to_string());

        graph.add_module(mypackage.clone());
        graph.add_module(mypackage_foo.clone());
        graph.add_module(mypackage_bar.clone());
        graph.add_module(mypackage_foo_alpha.clone());
        graph.add_module(mypackage_foo_alpha_blue.clone());
        graph.add_module(mypackage_foo_alpha_green.clone());
        graph.add_module(mypackage_foo_beta.clone());

        assert_eq!(
            graph.find_descendants(&mypackage_foo),
            HashSet::from([
                &mypackage_foo_alpha,
                &mypackage_foo_alpha_blue,
                &mypackage_foo_alpha_green,
                &mypackage_foo_beta
            ])
        );
    }

    #[test]
    fn direct_import_exists_returns_true() {
        let mut graph = Graph::default();
        let mypackage = Module::new("mypackage".to_string());
        let mypackage_foo = Module::new("mypackage.foo".to_string());
        let mypackage_bar = Module::new("mypackage.bar".to_string());
        graph.add_module(mypackage.clone());
        graph.add_module(mypackage_foo.clone());
        graph.add_module(mypackage_bar.clone());
        graph.add_import(&mypackage_foo, &mypackage_bar);

        assert!(graph.direct_import_exists(&mypackage_foo, &mypackage_bar));
    }

    #[test]
    fn direct_import_exists_returns_false() {
        let mut graph = Graph::default();
        let mypackage = Module::new("mypackage".to_string());
        let mypackage_foo = Module::new("mypackage.foo".to_string());
        let mypackage_bar = Module::new("mypackage.bar".to_string());
        graph.add_module(mypackage.clone());
        graph.add_module(mypackage_foo.clone());
        graph.add_module(mypackage_bar.clone());
        graph.add_import(&mypackage_foo, &mypackage_bar);

        assert!(!graph.direct_import_exists(&mypackage_bar, &mypackage_foo));
    }
}
