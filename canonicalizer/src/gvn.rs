//! This is the Global Value Numbering pass module.
//! It's job is to go through all the nodes cleaning up and dedublicating them.

use std::collections::HashMap;

struct GvnPass<'src, 'graph> {
    graph: &'graph Graph<'src>,
    /// `old ID -> new ID`
    data_cache: HashMap<DataNode<'src>, UData>,
    data: Vec<DataNode<'src>>,
}

impl<'src, 'graph> GvnPass<'src, 'graph> {
    fn process_data(&mut self, node: Data) -> UData {
        let data_node = self.graph[node].clone();
        if let Some(new) = self.data_cache.get(&data_node) {
            return *new;
        }

        let data_node = match data_node {
            any => any,
        };
        let data = UData(self.data.len());
        self.data.push(data_node.clone());
        self.data_cache.insert(data_node, data);
        data
    }
}
