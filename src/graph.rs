#![allow(dead_code)]

use bimap::BiMap;
use petgraph::stable_graph::{NodeIndex, StableGraph};
use petgraph::visit::{Bfs, Walker};
use petgraph::Direction;
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
    pub fn pretty_str(&self) -> String {
        let mut pretty = String::new();
        pretty.push_str("hierarchy:\n");
        let mut hierarchy_module_indices_sorted: Vec<_> =
            self.hierarchy_module_indices.iter().collect();
        hierarchy_module_indices_sorted.sort_by_key(|(_, index)| index.index());
        for (from_module, from_index) in hierarchy_module_indices_sorted {
            for to_index in self.hierarchy.neighbors(*from_index) {
                let to_module = self
                    .hierarchy_module_indices
                    .get_by_right(&to_index)
                    .unwrap();
                pretty.push_str(format!("  {} -> {}\n", from_module.name, to_module.name).as_str());
            }
        }

        pretty.push_str("imports:\n");
        let mut imports_module_indices_sorted: Vec<_> =
            self.imports_module_indices.iter().collect();
        imports_module_indices_sorted.sort_by_key(|(_, index)| index.index());
        for (from_module, from_index) in imports_module_indices_sorted {
            for to_index in self.imports.neighbors(*from_index) {
                let to_module = self.imports_module_indices.get_by_right(&to_index).unwrap();
                pretty.push_str(format!("  {} -> {}\n", from_module.name, to_module.name).as_str());
            }
        }
        pretty
    }

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

    pub fn add_import(&mut self, importer: &Module, imported: &Module) {
        self.add_module_if_not_in_hierarchy(&importer);
        self.add_module_if_not_in_hierarchy(&imported);

        let importer_index: NodeIndex = match self.imports_module_indices.get_by_left(&importer) {
            Some(index) => *index,
            None => {
                let index = self.imports.add_node(importer.clone());
                self.imports_module_indices.insert(importer.clone(), index);
                index
            }
        };
        let imported_index: NodeIndex = match self.imports_module_indices.get_by_left(&imported) {
            Some(index) => *index,
            None => {
                let index = self.imports.add_node(imported.clone());
                self.imports_module_indices.insert(imported.clone(), index);
                index
            }
        };

        self.imports.add_edge(importer_index, imported_index, ());
        // println!(
        //     "Added {:?} {:?} -> {:?} {:?}, edge count now {:?}",
        //     importer,
        //     importer_index,
        //     imported,
        //     imported_index,
        //     self.imports.edge_count()
        // );
    }

    #[allow(unused_variables)]
    pub fn direct_import_exists(
        &self,
        importer: &Module,
        imported: &Module,
        as_packages: bool,
    ) -> bool {
        // The modules may appear in the hierarchy, but have no imports, so we
        // return false unless they're both in there.
        let importer_index = match self.imports_module_indices.get_by_left(importer) {
            Some(importer_index) => *importer_index,
            None => return false,
        };
        let imported_index = match self.imports_module_indices.get_by_left(imported) {
            Some(imported_index) => *imported_index,
            None => return false,
        };

        self.imports.contains_edge(importer_index, imported_index)
    }

    pub fn find_modules_that_directly_import(&self, imported: &Module) -> HashSet<&Module> {
        let imported_index = *self.imports_module_indices.get_by_left(imported).unwrap();
        println!(
            "module, {:?}, imported_index {:?}",
            imported, imported_index
        );
        let importer_indices: HashSet<NodeIndex> = self
            .imports
            .neighbors_directed(imported_index, Direction::Incoming)
            .collect();

        println!("importer indices {:?}", importer_indices);
        for i in importer_indices.iter() {
            println!(
                "Importer {:?}",
                self.imports_module_indices.get_by_right(&i).unwrap()
            );
        }
        let importers: HashSet<&Module> = importer_indices
            .iter()
            .map(|importer_index| {
                self.imports_module_indices
                    .get_by_right(&importer_index)
                    .unwrap()
            })
            .collect();
        importers
    }

    pub fn find_modules_directly_imported_by(&self, importer: &Module) -> HashSet<&Module> {
        let importer_index = *self.imports_module_indices.get_by_left(importer).unwrap();
        let imported_indices: HashSet<NodeIndex> = self
            .imports
            .neighbors_directed(importer_index, Direction::Outgoing)
            .collect();

        let importeds: HashSet<&Module> = imported_indices
            .iter()
            .map(|imported_index| {
                self.imports_module_indices
                    .get_by_right(&imported_index)
                    .unwrap()
            })
            .collect();
        importeds
    }

    #[allow(unused_variables)]
    pub fn squash_module(&mut self, module: &Module) {
        // Get descendants and their imports.
        let descendants = self.find_descendants(module);
        let modules_imported_by_descendants: Vec<Module> = descendants
            .iter()
            .flat_map(|descendant| {
                self.find_modules_directly_imported_by(descendant)
                    .into_iter()
                    .cloned()
            })
            .collect();
        let modules_that_import_descendants: Vec<Module> = descendants
            .iter()
            .flat_map(|descendant| {
                self.find_modules_that_directly_import(descendant)
                    .into_iter()
                    .cloned()
            })
            .collect();

        // Remove descendants.
        for descendant in descendants {
            let descendant_hierarchy_index = self
                .hierarchy_module_indices
                .get_by_left(&descendant)
                .unwrap();
            // self.hierarchy
            //     .remove_node(descendant_hierarchy_index.clone());
            // self.hierarchy_module_indices.remove_by_left(descendant);

            let descendant_imports_index = self
                .imports_module_indices
                .get_by_left(&descendant)
                .unwrap();
            // self.imports.remove_node(descendant_imports_index.clone());
            // self.imports_module_indices.remove_by_left(descendant);
        }

        // Add descendants and imports to parent module.
        for imported in modules_imported_by_descendants {
            self.add_import(module, &imported);
        }

        for importer in modules_that_import_descendants {
            self.add_import(&importer, module);
        }
    }

    fn add_module_if_not_in_hierarchy(&mut self, module: &Module) {
        if self.hierarchy_module_indices.get_by_left(&module).is_none() {
            self.add_module(module.clone());
        };
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
        assert_eq!(
            graph.pretty_str(),
            "
hierarchy:
  mypackage -> mypackage.foo
imports:
"
            .trim_start()
        );
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

        assert!(graph.direct_import_exists(&mypackage_foo, &mypackage_bar, false));
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

        assert!(!graph.direct_import_exists(&mypackage_bar, &mypackage_foo, false));
    }

    #[test]
    fn direct_import_exists_returns_false_root_to_child() {
        let mut graph = Graph::default();
        let mypackage = Module::new("mypackage".to_string());
        let mypackage_foo = Module::new("mypackage.foo".to_string());
        let mypackage_bar = Module::new("mypackage.bar".to_string());
        let mypackage_foo_alpha = Module::new("mypackage.foo.alpha".to_string());
        graph.add_module(mypackage.clone());
        graph.add_module(mypackage_foo.clone());
        graph.add_module(mypackage_bar.clone());
        graph.add_module(mypackage_foo_alpha.clone());
        graph.add_import(&mypackage_bar, &mypackage_foo_alpha);

        assert_eq!(
            graph.pretty_str(),
            "
hierarchy:
  mypackage -> mypackage.bar
  mypackage -> mypackage.foo
  mypackage.foo -> mypackage.foo.alpha
imports:
  mypackage.bar -> mypackage.foo.alpha
"
            .trim_start()
        );
        assert!(!graph.direct_import_exists(&mypackage_bar, &mypackage_foo, false));
    }

    #[test]
    fn add_import_with_non_existent_importer_adds_that_module() {
        let mut graph = Graph::default();
        let mypackage = Module::new("mypackage".to_string());
        let mypackage_foo = Module::new("mypackage.foo".to_string());
        let mypackage_bar = Module::new("mypackage.bar".to_string());
        graph.add_module(mypackage_bar.clone());

        graph.add_import(&mypackage_foo, &mypackage_bar);

        assert_eq!(
            graph.get_modules(),
            HashSet::from([&mypackage, &mypackage_bar, &mypackage_foo])
        );
        assert!(graph.direct_import_exists(&mypackage_foo, &mypackage_bar, false));
        assert_eq!(
            graph.pretty_str(),
            "
hierarchy:
  mypackage -> mypackage.foo
  mypackage -> mypackage.bar
imports:
  mypackage.foo -> mypackage.bar
"
            .trim_start()
        );
    }

    #[test]
    fn add_import_with_non_existent_imported_adds_that_module() {
        let mut graph = Graph::default();
        let mypackage = Module::new("mypackage".to_string());
        let mypackage_foo = Module::new("mypackage.foo".to_string());
        let mypackage_bar = Module::new("mypackage.bar".to_string());
        graph.add_module(mypackage_foo.clone());

        graph.add_import(&mypackage_foo, &mypackage_bar);

        assert_eq!(
            graph.get_modules(),
            HashSet::from([&mypackage, &mypackage_bar, &mypackage_foo])
        );
        assert!(graph.direct_import_exists(&mypackage_foo, &mypackage_bar, false));
        assert_eq!(
            graph.pretty_str(),
            "
hierarchy:
  mypackage -> mypackage.bar
  mypackage -> mypackage.foo
imports:
  mypackage.foo -> mypackage.bar
"
            .trim_start()
        );
    }

    //#[test]
    // TODO: get squash_module working first
    fn direct_import_exists_with_as_packages_returns_false() {
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
        // Add an import in the other direction.
        graph.add_import(&mypackage_bar, &mypackage_foo);

        assert!(!graph.direct_import_exists(&mypackage_foo, &mypackage_bar, true));
    }

    #[test]
    fn direct_import_exists_with_as_packages_returns_true_between_roots() {
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
        graph.add_import(&mypackage_foo, &mypackage_bar);

        assert!(graph.direct_import_exists(&mypackage_foo, &mypackage_bar, true));
    }

    //#[test]
    // TODO: get squash_module working first
    fn direct_import_exists_with_as_packages_returns_true_root_to_child() {
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
        graph.add_import(&mypackage_bar, &mypackage_foo_alpha);

        assert!(graph.direct_import_exists(&mypackage_bar, &mypackage_foo, true));
    }

    //#[test]
    // TODO: get squash_module working first
    fn direct_import_exists_with_as_packages_returns_true_child_to_root() {
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
        graph.add_import(&mypackage_foo_alpha, &mypackage_bar);

        assert!(graph.direct_import_exists(&mypackage_foo, &mypackage_bar, true));
    }

    #[test]
    fn find_modules_that_directly_import() {
        let mut graph = Graph::default();
        let mypackage = Module::new("mypackage".to_string());
        let mypackage_foo = Module::new("mypackage.foo".to_string());
        let mypackage_bar = Module::new("mypackage.bar".to_string());
        let mypackage_foo_alpha = Module::new("mypackage.foo.alpha".to_string());
        let mypackage_foo_alpha_blue = Module::new("mypackage.foo.alpha.blue".to_string());
        let mypackage_foo_alpha_green = Module::new("mypackage.foo.alpha.green".to_string());
        let mypackage_foo_beta = Module::new("mypackage.foo.beta".to_string());
        let anotherpackage = Module::new("anotherpackage".to_string());
        graph.add_module(mypackage.clone());
        graph.add_module(mypackage_foo.clone());
        graph.add_module(mypackage_bar.clone());
        graph.add_module(mypackage_foo_alpha.clone());
        graph.add_module(mypackage_foo_alpha_blue.clone());
        graph.add_module(mypackage_foo_alpha_green.clone());
        graph.add_module(mypackage_foo_beta.clone());
        graph.add_import(&mypackage_foo_alpha, &mypackage_bar);
        graph.add_import(&anotherpackage, &mypackage_bar);
        graph.add_import(&mypackage_bar, &mypackage_foo_alpha_green);

        let result = graph.find_modules_that_directly_import(&mypackage_bar);

        assert_eq!(
            result,
            HashSet::from([&mypackage_foo_alpha, &anotherpackage])
        )
    }

    #[test]
    fn find_modules_directly_imported_by() {
        let mut graph = Graph::default();
        let mypackage = Module::new("mypackage".to_string());
        let mypackage_foo = Module::new("mypackage.foo".to_string());
        let mypackage_bar = Module::new("mypackage.bar".to_string());
        let mypackage_foo_alpha = Module::new("mypackage.foo.alpha".to_string());
        let mypackage_foo_alpha_blue = Module::new("mypackage.foo.alpha.blue".to_string());
        let mypackage_foo_alpha_green = Module::new("mypackage.foo.alpha.green".to_string());
        let mypackage_foo_beta = Module::new("mypackage.foo.beta".to_string());
        let anotherpackage = Module::new("anotherpackage".to_string());
        graph.add_module(mypackage.clone());
        graph.add_module(mypackage_foo.clone());
        graph.add_module(mypackage_bar.clone());
        graph.add_module(mypackage_foo_alpha.clone());
        graph.add_module(mypackage_foo_alpha_blue.clone());
        graph.add_module(mypackage_foo_alpha_green.clone());
        graph.add_module(mypackage_foo_beta.clone());
        graph.add_import(&mypackage_bar, &mypackage_foo_alpha);
        graph.add_import(&mypackage_bar, &anotherpackage);
        graph.add_import(&mypackage_foo_alpha_green, &mypackage_bar);

        let result = graph.find_modules_directly_imported_by(&mypackage_bar);

        assert_eq!(
            result,
            HashSet::from([&mypackage_foo_alpha, &anotherpackage])
        )
    }

    #[test]
    fn squash_module() {
        let mut graph = Graph::default();
        let mypackage = Module::new("mypackage".to_string());
        let mypackage_foo = Module::new("mypackage.foo".to_string());
        let mypackage_bar = Module::new("mypackage.bar".to_string());
        let mypackage_foobar = Module::new("mypackage.foobar".to_string());
        let mypackage_foo_alpha = Module::new("mypackage.foo.alpha".to_string());
        let mypackage_foo_alpha_blue = Module::new("mypackage.foo.alpha.blue".to_string());
        let mypackage_foo_beta = Module::new("mypackage.foo.beta".to_string());
        let mypackage_bar_beta = Module::new("mypackage.bar.beta".to_string());
        graph.add_module(mypackage_foo.clone());
        graph.add_module(mypackage_bar.clone());
        graph.add_module(mypackage_foo_alpha.clone());
        graph.add_module(mypackage_foo_alpha_blue.clone());
        graph.add_module(mypackage_foo_beta.clone());
        graph.add_import(&mypackage_foo_alpha, &mypackage_bar_beta);
        graph.add_import(&mypackage_foo_alpha, &mypackage_bar_beta);
        graph.add_import(&mypackage_foobar, &mypackage_foo_beta);

        graph.squash_module(&mypackage_foo);

        assert_eq!(
            graph.pretty_str(),
            "
hierarchy:
  mypackage -> mypackage.bar
  mypackage -> mypackage.foobar
  mypackage -> mypackage.foo
  mypackage.bar -> mypackage.bar.beta
imports:
  mypackage.foobar -> mypackage.foo
  mypackage.foo -> mypackage.bar.beta
"
            .trim_start()
        );
        assert_eq!(
            graph.get_modules(),
            HashSet::from([
                &mypackage,
                &mypackage_foo,
                &mypackage_bar,
                &mypackage_bar_beta
            ])
        );
        assert!(graph.direct_import_exists(&mypackage_foo, &mypackage_bar_beta, false));
        assert!(graph.direct_import_exists(&mypackage_foobar, &mypackage_foo, false));
    }
}
