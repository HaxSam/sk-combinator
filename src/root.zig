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

const Operator = union(enum) { comb: Combinator, vari: u8, unknown: void };

const Expr = struct {
    const Self = @This();

    op: Operator,
    stack: std.ArrayList(Self),

    pub fn init(op: Operator, allocator: Allocator) Self {
        return Self{
            .op = op,
            .stack = .init(allocator),
        };
    }

    pub fn initWithStack(op: Operator, stack: []const Expr, allocator: Allocator) !Self {
        var s: std.ArrayList(Self) = try .initCapacity(allocator, stack.len);
        for (stack) |expr|
            try s.insert(0, try expr.clone());

        return Self{
            .op = op,
            .stack = s,
        };
    }

    pub fn initWithStackOp(op: Operator, stack: []const Operator, allocator: Allocator) !Self {
        var s: std.ArrayList(Self) = try .initCapacity(allocator, stack.len);
        for (stack) |oper|
            try s.insert(0, init(oper, allocator));

        return Self{
            .op = op,
            .stack = s,
        };
    }

    pub fn initWithStackComb(op: Combinator, stack: []const Combinator, allocator: Allocator) !Self {
        var s: std.ArrayList(Self) = try .initCapacity(allocator, stack.len);
        for (stack) |oper|
            try s.insert(0, init(.{ .comb = oper }, allocator));

        return Self{
            .op = .{ .comb = op },
            .stack = s,
        };
    }

    pub fn number(n: usize, allocator: Allocator) !Self {
        var res = init(.{ .unknown = {} }, allocator);
        var cur = &res;

        op: switch (n) {
            0 => {
                cur.op = .{ .comb = .A };
                return res;
            },
            1 => {
                cur.op = .{ .comb = .I };
                return res;
            },
            2 => {
                cur.op = .{ .comb = .W };
                try cur.appendEndComb(.B);
                return res;
            },
            3 => {
                cur.op = .{ .comb = .S };
                try cur.appendEndExpr(try initWithStackComb(.W, ([_]Combinator{.B})[0..], allocator));
                try cur.appendEndComb(.B);
                return res;
            },
            else => |count| {
                if (count % 2 == 1) {
                    cur.op = .{ .comb = .S };
                    try cur.appendEndExpr(init(.{ .unknown = {} }, allocator));
                    try cur.appendEndComb(.B);
                    cur = &cur.stack.items[0];
                }

                cur.op = .{ .comb = .B };
                try cur.appendEndExpr(init(.{ .unknown = {} }, allocator));
                try cur.appendEndExpr(try initWithStackComb(.W, ([_]Combinator{.B})[0..], allocator));
                cur = &cur.stack.items[0];

                continue :op count / 2;
            },
        }
    }

    pub fn parser(str: []const u8, allocator: Allocator) !Self {
        var stack: std.ArrayList(Expr) = .init(allocator);
        defer stack.deinit();
        var root: ?Expr = null;
        var num: ?usize = null;

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
            '0'...'9' => |c| {
                const char_number = @as(usize, c - 0x30);
                if (num) |*n| {
                    n.* = char_number + 10 * n.*;
                } else {
                    num = char_number;
                }

                i += 1;
                if (i != str.len) {
                    if (!(str[i] >= '0' and str[i] <= '9')) {
                        if (root) |*expr| {
                            try expr.appendStartExpr(try number(num.?, allocator));
                        } else {
                            root = try number(num.?, allocator);
                        }
                        num = null;
                    }
                    continue :op str[i];
                }
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
                    } else {
                        var pre_root = stack.pop().?;
                        try pre_root.appendStartExpr(try number(num.?, allocator));
                        root = pre_root;
                    }
                }

                if (i != str.len)
                    continue :op str[i];
            },
            '+' => {
                if (root) |*expr| {
                    try expr.appendStartExpr(try initWithStackComb(.S, ([_]Combinator{.B})[0..], allocator));
                } else {
                    root = init(.{ .comb = .S }, allocator);
                    try root.?.appendEndComb(.B);
                }

                i += 1;
                if (i != str.len)
                    continue :op str[i];
            },
            else => {
                i += 1;
                if (i != str.len)
                    continue :op str[i];
            },
        }

        if (num) |n| {
            if (root) |*expr| {
                try expr.appendStartExpr(try number(n, allocator));
            } else {
                root = try number(n, allocator);
            }
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
            .unknown => return 255,
        }
    }

    pub fn format(self: Self, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("{c}", .{self.char()});

        var i = self.stack.items.len;
        while (i > 0) : (i -= 1) {
            const expr = self.stack.items[i - 1];
            if (expr.stack.items.len != 0)
                try writer.writeAll("(");
            try expr.format(fmt, options, writer);
            try writer.writeAll(if (expr.stack.items.len != 0) ")" else "");
        }
    }

    pub fn appendEnd(self: *Self, op: Operator) !void {
        try self.stack.append(Self{
            .op = op,
            .stack = .init(self.stack.allocator),
        });
    }

    pub fn appendEndComb(self: *Self, op: Combinator) !void {
        try self.appendEnd(.{ .comb = op });
    }

    pub fn appendEndVari(self: *Self, op: u8) !void {
        try self.appendEnd(.{ .vari = op });
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
        try self.appendStart(.{ .comb = op });
    }

    pub fn appendStartVari(self: *Self, op: u8) !void {
        try self.appendStart(.{ .vari = op });
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
        const ret = Self{
            .op = self.op,
            .stack = try self.stack.clone(),
        };

        for (ret.stack.items) |*expr| {
            expr.* = try expr.clone();
        }

        return ret;
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

                    var Y = init(.{ .comb = .Y }, self.stack.allocator);
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
        while (i > 0) : (i -= 1) {
            if (try self.stack.items[i - 1].stepNormal())
                return true;
        }

        return false;
    }
};

test "idk" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var expr = try Expr.parser("+(+(+0))fx", allocator);
    defer expr.deinit();

    std.debug.print("{}\n", .{expr});

    //while (try expr.stepNormal()) {
    //    std.debug.print("{}\n", .{expr});
    //}

    for (0..5) |_| {
        _ = try expr.stepNormal();
        std.debug.print("{}\n", .{expr});
    }
}
