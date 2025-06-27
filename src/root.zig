const std = @import("std");
const Allocator = std.mem.Allocator;

const Combinator = enum(u8) {
    S = 'S',
    K = 'K',
    I = 'I',
    B = 'B',
    C = 'C',
    A = 'A',
    M = 'M',
    T = 'T',
    W = 'W',
    Y = 'Y',
};

const Operator = union(enum) { comb: Combinator, vari: u8 };

const Expr = struct {
    const Self = @This();

    op: Operator,
    stack: std.ArrayList(Expr),

    pub fn init(op: Operator, allocator: Allocator) Self {
        return Self{
            .op = op,
            .stack = .init(allocator),
        };
    }

    pub fn parser(str: []const u8, allocator: Allocator) !Self {
        var stack: std.ArrayList(Expr) = .init(allocator);
        defer stack.deinit();
        var root: ?Expr = null;

        var i: usize = 0;
        op: switch (str[i]) {
            'S', 'K', 'I', 'B', 'C', 'A', 'M', 'T', 'W', 'Y' => |comb| {
                if (root) |*expr| {
                    try expr.appendStartComb(@enumFromInt(comb));
                } else {
                    root = init(.{ .comb = @enumFromInt(comb) }, allocator);
                }

                i += 1;
                if (i != str.len)
                    continue :op str[i];
            },
            'a'...'z' => |vari| {
                if (root) |*expr| {
                    try expr.appendStartVari(vari);
                } else {
                    root = init(.{ .vari = vari }, allocator);
                }

                i += 1;
                if (i != str.len)
                    continue :op str[i];
            },
            '(' => {
                if (root) |expr| {
                    try stack.append(expr);
                    root = null;
                }

                i += 1;
                if (i != str.len)
                    continue :op str[i];
            },
            ')' => {
                i += 1;
                if (stack.items.len != 0) {
                    if (root) |expr| {
                        var pre_root = stack.pop().?;
                        try pre_root.appendStartExpr(expr);
                        root = pre_root;
                    }
                }

                if (i != str.len)
                    continue :op str[i];
            },
            else => {
                i += 1;
                if (i != str.len)
                    continue :op str[i];
            },
        }

        return root.?;
    }

    pub fn deinit(self: *const Self) void {
        for (self.stack.items) |expr| {
            expr.deinit();
        }
        self.stack.deinit();
    }

    pub fn char(self: *const Self) u8 {
        switch (self.op) {
            .comb => |comb| return @intFromEnum(comb),
            .vari => |vari| return vari,
        }
    }

    pub fn appendEnd(self: *Self, op: Operator) !void {
        try self.stack.append(Self{
            .op = op,
            .stack = .init(self.stack.allocator),
        });
    }

    pub fn appendEndComb(self: *Self, op: Combinator) !void {
        try self.stack.append(Self{
            .op = .{ .comb = op },
            .stack = .init(self.stack.allocator),
        });
    }

    pub fn appendEndVari(self: *Self, op: u8) !void {
        try self.stack.append(Self{
            .op = .{ .vari = op },
            .stack = .init(self.stack.allocator),
        });
    }

    pub fn appendEndExpr(self: *Self, expr: Expr) !void {
        try self.stack.append(expr);
    }

    pub fn appendStart(self: *Self, op: Operator) !void {
        try self.stack.insert(0, Self{
            .op = op,
            .stack = .init(self.stack.allocator),
        });
    }

    pub fn appendStartComb(self: *Self, op: Combinator) !void {
        try self.stack.insert(0, Self{
            .op = .{ .comb = op },
            .stack = .init(self.stack.allocator),
        });
    }

    pub fn appendStartVari(self: *Self, op: u8) !void {
        try self.stack.insert(0, Self{
            .op = .{ .vari = op },
            .stack = .init(self.stack.allocator),
        });
    }

    pub fn appendStartExpr(self: *Self, expr: Expr) !void {
        try self.stack.insert(0, expr);
    }

    pub fn pop1(self: *Self) ?Expr {
        return self.stack.pop();
    }

    pub fn pop2(self: *Self) ?struct { Expr, Expr } {
        if (self.stack.items.len < 2)
            return null;

        return .{ self.stack.pop().?, self.stack.pop().? };
    }

    pub fn pop3(self: *Self) ?struct { Expr, Expr, Expr } {
        if (self.stack.items.len < 3)
            return null;

        return .{ self.stack.pop().?, self.stack.pop().?, self.stack.pop().? };
    }

    pub fn clone(self: *const Self) !Self {
        return Self{
            .op = self.op,
            .stack = try self.stack.clone(),
        };
    }

    pub fn step(self: *Self) !bool {
        var x: Expr = undefined;

        switch (self.op) {
            .comb => |*comb| switch (comb.*) {
                .S => {
                    x, var y, const z = self.pop3() orelse return false;

                    try y.appendStartExpr(z);
                    try self.appendEndExpr(y);
                    try self.appendEndExpr(try z.clone());
                },
                .K => {
                    x, const y = self.pop2() orelse return false;

                    y.deinit();
                },
                .I => {
                    x = self.pop1() orelse return false;
                },
                .B => {
                    x, var y, const z = self.pop3() orelse return false;

                    try y.appendStartExpr(z);
                    try self.appendEndExpr(y);
                },
                .C => {
                    x, const y, const z = self.pop3() orelse return false;

                    try self.appendEndExpr(y);
                    try self.appendEndExpr(z);
                },
                .A => {
                    const y, x = self.pop2() orelse return false;

                    y.deinit();
                },
                .M => {
                    x = self.pop1() orelse return false;

                    try self.appendEndExpr(try x.clone());
                },
                .T => {
                    const y, x = self.pop2() orelse return false;

                    try self.appendEndExpr(y);
                },
                .W => {
                    x, const y = self.pop2() orelse return false;

                    try self.appendEndExpr(y);
                    try self.appendEndExpr(try y.clone());
                },
                .Y => {
                    x = self.pop1() orelse return false;

                    var Y = Expr.init(.{ .comb = .Y }, self.stack.allocator);
                    try Y.appendEndExpr(try x.clone());
                    try self.appendEndExpr(Y);
                },
            },
            else => return false,
        }

        self.op = x.op;
        try self.stack.appendSlice(x.stack.items);
        x.stack.deinit();
        return true;
    }

    pub fn stepNormal(self: *Self) !bool {
        if (try self.step())
            return true;

        var i = self.stack.items.len;
        while (i > 0) : (i -= 1) {}
    }
};

test "idk" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var expr = try Expr.parser("Yx", allocator);
    defer expr.deinit();

    //try expr.appendComb(.S);

    _ = try expr.step();
    _ = try expr.step();
    _ = try expr.step();
    _ = try expr.step();
    std.debug.print("{c}\n", .{expr.char()});
    for (expr.stack.items) |i| {
        if (i.stack.items.len == 0)
            std.debug.print("{c}\n", .{i.char()});
        for (i.stack.items) |j| {
            std.debug.print("{c} {c}\n", .{ i.char(), j.char() });
        }
    }
}
