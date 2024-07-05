use std::collections::HashSet;

#[derive(Debug, PartialEq, Eq, Hash)]
struct Module<'a> {
    name: String,
    parent: Option<&'a Module<'a>>,
}

struct ModuleGraph<'a> {
    pub modules: HashSet<&'a Module<'a>>,
}

impl<'a> ModuleGraph<'a> {
    pub fn new() -> ModuleGraph<'a> {
        ModuleGraph {
            modules: HashSet::new(),
        }
    }

    pub fn add_module(&mut self, module: &'a Module) {
        self.modules.insert(module);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new() {
        let graph = ModuleGraph::new();

        assert_eq!(graph.modules, HashSet::new());
    }

    #[test]
    fn add_module() {
        let mut graph = ModuleGraph::new();
        let mypackage = Module {
            name: "mypackage".to_string(),
            parent: None,
        };

        graph.add_module(&mypackage);

        assert_eq!(graph.modules, HashSet::from([&mypackage]));
    }

    #[test]
    fn add_modules() {
        let mut graph = ModuleGraph::new();
        let mypackage = Module {
            name: "mypackage".to_string(),
            parent: None,
        };
        let mypackage_foo = Module {
            name: "mypackage.foo".to_string(),
            parent: Some(&mypackage),
        };

        graph.add_module(&mypackage);
        graph.add_module(&mypackage_foo);

        assert_eq!(graph.modules, HashSet::from([&mypackage, &mypackage_foo]));
    }
}
