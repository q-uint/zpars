/// Bounded bump-allocator pool backed by a fixed-size array.
///
/// Used by grammar parsers to allocate AST nodes, rules, byte sequences,
/// and diagnostics without a heap allocator.
pub fn Pool(comptime T: type, comptime capacity: usize) type {
    return struct {
        items: [capacity]T = undefined,
        count: usize = 0,

        /// Append one item; return a pointer into the pool.
        pub fn addOne(self: *@This(), item: T) *const T {
            self.items[self.count] = item;
            const ptr = &self.items[self.count];
            self.count += 1;
            return ptr;
        }

        /// Append a slice of items; return a sub-slice into the pool.
        pub fn addSlice(self: *@This(), items: []const T) []const T {
            const start = self.count;
            for (items) |item| {
                self.items[self.count] = item;
                self.count += 1;
            }
            return self.items[start..self.count];
        }

        /// Return the populated portion of the pool.
        pub fn slice(self: *const @This()) []const T {
            return self.items[0..self.count];
        }
    };
}
