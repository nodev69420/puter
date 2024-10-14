local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
	vim.fn.system({
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/folke/lazy.nvim.git",
		"--branch=stable",
		lazypath,
	})

end
vim.opt.rtp:prepend(lazypath)
vim.g.mapleader = " "
vim.g.zig_fmt_autosave = 0
vim.o.clipboard = "unnamedplus"
vim.opt.mouse = ""
vim.opt.list = true
vim.opt.lcs = "tab:> ,lead:.,trail:.,eol:$"
vim.opt.completeopt = {"menu", "menuone", "noselect", "preview"}
vim.opt.nu = true
vim.opt.relativenumber = true
vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = false
vim.opt.preserveindent = true
vim.opt.wrap = false
vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.hlsearch = false
vim.opt.incsearch = true
vim.opt.splitright = true
vim.opt.splitbelow = true
vim.opt.termguicolors = true
vim.opt.scrolloff = 16
vim.opt.signcolumn = "yes"
vim.opt.colorcolumn = "80"
vim.opt.updatetime = 50
vim.lsp.set_log_level("off")

vim.g.makeprg = "zig build run"

require("lazy").setup({
	{
		"folke/neoconf.nvim",
		cmd = "Neoconf"
	},
	"folke/neodev.nvim",
	"folke/trouble.nvim",
	-- Treesitter
	"nvim-treesitter/nvim-treesitter",
	"nvim-telescope/telescope.nvim",
	"nvim-lua/plenary.nvim",
	"LukasPietzschmann/telescope-tabs",
	-- LSP
	"VonHeikemen/lsp-zero.nvim",
	"williamboman/mason.nvim",
	"williamboman/mason-lspconfig.nvim",
	"neovim/nvim-lspconfig",
	"hrsh7th/cmp-nvim-lsp",
	"hrsh7th/nvim-cmp",
	'L3MON4D3/LuaSnip',
	"VonHeikemen/lsp-zero.nvim",
	-- DAP
	"mfussenegger/nvim-dap",
	"rcarriga/nvim-dap-ui",
	"nvim-neotest/nvim-nio",
	"nvim-telescope/telescope-dap.nvim",
	-- Language specific
	"Civitasv/cmake-tools.nvim",
	"NTBBloodbath/zig-tools.nvim",
	-- Other
	"akinsho/toggleterm.nvim",
	"nvim-neotest/nvim-nio",
	"ThePrimeagen/harpoon",
	"tpope/vim-fugitive",
	"lewis6991/gitsigns.nvim",
	"stevearc/aerial.nvim",
	"stevearc/oil.nvim",
	"RaafatTurki/hex.nvim",
	"echasnovski/mini.pairs",
	"nvim-tree/nvim-web-devicons",
	"bluz71/vim-moonfly-colors",
	"patstockwell/vim-monokai-tasty",
	"diegoulloao/neofusion.nvim",
})
require("trouble").setup()
require("nvim-treesitter.configs").setup({
	ensure_installed = {"c", "lua", "vim", "vimdoc", "query"},
	sync_install = true,
	auto_install = true,
	highlight = {
		enable = true,
		additional_vim_regex_highlighting = false,
	},
})
require("telescope").setup({
	defaults = {
		layout_config = {
			bottom_pane = {
				height = 100,
				preview_cutoff = 100,
				prompt_position = "bottom"
			},
		},
		layout_strategy = "bottom_pane",
		border = true
	}
})
require("telescope").load_extension("dap")
require("telescope").load_extension("telescope-tabs")
require("telescope").load_extension("harpoon")
require("telescope").load_extension("aerial")
require("telescope-tabs").setup()
local lsp_zero = require("lsp-zero")
lsp_zero.on_attach(function(client, bufnr)
	local opts = {buffer = bufnr, remap = false}
	vim.keymap.set("n", "gd", function() vim.lsp.buf.definition() end, opts)
	vim.keymap.set("n", "K", function() vim.lsp.buf.hover() end, opts)
	vim.keymap.set("n", "<leader>vk", function()
		vim.lsp.buf.workspace_symbol()
	end, opts)
	vim.keymap.set("n", "<leader>vq", function()
		vim.diagnostic.open_float()
	end, opts)
	vim.keymap.set("n", "[d", function() vim.diagnostic.goto_next() end, opts)
	vim.keymap.set("n", "]d", function() vim.diagnostic.goto_prev() end, opts)
	vim.keymap.set("n", "<leader>vc", function()
		vim.lsp.buf.code_action()
	end, opts)
	vim.keymap.set("n", "<leader>vr", function()
		vim.lsp.buf.references()
	end, opts)
	vim.keymap.set("n", "<leader>vn", function()
		vim.lsp.buf.rename()
	end, opts)
	vim.keymap.set("i", "<C-h>", function()
		vim.lsp.buf.signature_help()
	end, opts)
end)
require("mason").setup({})
require("mason-lspconfig").setup({
	ensure_installed = {
		"clangd",
		"lua_ls",
		"bashls",
	},
	handlers = {
		lsp_zero.default_setup,
		lua_ls = function()
			local lua_opts = lsp_zero.nvim_lua_ls()
			require("lspconfig").lua_ls.setup(lua_opts)
		end,
	}
})
local cmp = require("cmp")
local cmp_select = {behavior = cmp.SelectBehavior.Select}
cmp.setup({
	sources = {
		{name = "nvim_lsp"},
	},
	formatting = lsp_zero.cmp_format(),
	mapping = cmp.mapping.preset.insert({
		['<C-u>'] = cmp.mapping.scroll_docs(-4),
		['<C-d>'] = cmp.mapping.scroll_docs(4),
		["<C-p>"] = cmp.mapping.select_prev_item(cmp_select),
		["<C-n>"] = cmp.mapping.select_next_item(cmp_select),
		["<C-k>"] = cmp.mapping.select_prev_item(cmp_select),
		["<C-j>"] = cmp.mapping.select_next_item(cmp_select),
		["<C-l>"] = cmp.mapping.confirm({ select = true }),
		["<C-;>"] = cmp.mapping.confirm({ select = true }),
		["<C-Enter>"] = cmp.mapping.complete(),
	}),
	snippet = {
		expand = function(args)
			require("luasnip").lsp_expand(args.body)
		end
	}
})
local luasnip = require("luasnip")
luasnip.config.set_config({
	history = false,
	updateevents = "TextChanged,TextChangedI"
})
-- DAP
local dap = require("dap")
dap.adapters.gdb = {
	type = "executable",
	command = "gdb",
	args = { "-i", "dap" }
}
dap.configurations.cpp = {
	{
		name = "GDB Native",
		type = "gdb",
		request = "launch",
		program = function()
			return vim.fn.input("Target: ", vim.fn.getcwd() .. "/", "file")
		end,
		cwd = "${workspaceFolder}",
		stopAtBeginningOfMainSubprogram = false,
	},
}
dap.configurations.c = dap.configurations.cpp;
require("dapui").setup({
	controls = {
		element = "repl",
		enabled = true,
		icons = {
			disconnect = "",
			pause = "",
			play = "",
			run_last = "",
			step_back = "",
			step_into = "",
			step_out = "",
			step_over = "",
			terminate = ""
		}
	},
	element_mappings = {},
	expand_lines = true,
	floating = {
		border = "single",
		mappings = {
			close = { "q", "<Esc>" }
		}
	},
	force_buffers = true,
	icons = {
		collapsed = "",
		current_frame = "",
		expanded = ""
	},
	layouts = {
		{
			elements = {
				{
					id = "watches",
					size = 0.5
				},
				{
					id = "scopes",
					size = 0.5
				},
			},
			position = "left",
			size = 80
		},
	},
	mappings = {
		edit = "e",
		expand = { "<CR>", "<2-LeftMouse>" },
		open = "o",
		remove = "d",
		repl = "r",
		toggle = "t"
	},
	render = {
		indent = 0,
		max_value_lines = 100
	}
})
-- CMake
local osys = require("cmake-tools.osys")
require("cmake-tools").setup({
	cmake_command = "cmake",
	ctest_command = "ctest",
	cmake_use_preset = true,
	cmake_regenerate_on_save = true,
	cmake_generate_options = { "-DCMAKE_EXPORT_COMPILE_COMMANDS=1" },
	cmake_build_options = {},
	-- support macro expansion:
	--       ${kit}
	--       ${kitGenerator}
	--       ${variant:xx}
	cmake_build_directory = function()
		if osys.iswin32 then
			return "out\\${variant:buildType}"
		end
		return "out/${variant:buildType}"
	end,
	cmake_soft_link_compile_commands = true,
	cmake_compile_commands_from_lsp = true,
	cmake_kits_path = nil,
	cmake_variants_message = {
		short = { show = true },
		long = { show = true, max_length = 40 },
	},
	cmake_dap_configuration = {
		name = "GDB Native",
		type = "gdb",
		request = "launch",
		stopAtBeginningOfMainSubprogram = false,
	},
	cmake_executor = {
		name = "quickfix",
		opts = {},
		default_opts = {
			quickfix = {
				show = "always",
				position = "belowright",
				size = 10,
				encoding = "utf-8",
				auto_close_when_success = true,
			},
			toggleterm = {
				direction = "float",
				close_on_exit = false,
				auto_scroll = true,
				singleton = true,
			},
			overseer = {
				new_task_opts = {
					strategy = {
						"toggleterm",
						direction = "horizontal",
						autos_croll = true,
						quit_on_exit = "success"
					}
				},
				on_new_task = function(task)
					require("overseer").open({
						enter = false, direction = "right"
					})
				end,
			},
			terminal = {
				name = "Main Terminal",
				prefix_name = "[CMakeTools]: ",
				split_direction = "horizontal",
				split_size = 11,
				single_terminal_per_instance = true,
				single_terminal_per_tab = true,
				keep_terminal_static_location = true,
				start_insert = false,
				focus = false,
				do_not_add_newline = false,
			},
		},
	},
	cmake_runner = {
		name = "terminal",
		opts = {},
		default_opts = {
			quickfix = {
				show = "always",
				position = "belowright",
				size = 10,
				encoding = "utf-8",
				auto_close_when_success = true,
			},
			toggleterm = {
				direction = "float",
				close_on_exit = false,
				auto_scroll = true,
				singleton = true,
			},
			overseer = {
				new_task_opts = {
					strategy = {
						"toggleterm",
						direction = "horizontal",
						autos_croll = true,
						quit_on_exit = "success"
					}
				},
				on_new_task = function(task)
				end,
			},
			terminal = {
				name = "Main Terminal",
				prefix_name = "[CMakeTools]: ",
				split_direction = "horizontal",
				split_size = 11,
				single_terminal_per_instance = true,
				single_terminal_per_tab = true,
				keep_terminal_static_location = true,
				start_insert = false,
				focus = false,
				do_not_add_newline = false,
			},
		},
	},
	cmake_notifications = {
		runner = { enabled = true },
		executor = { enabled = true },
		spinner = { "⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏" },
		refresh_rate_ms = 100,
	},
	cmake_virtual_text_support = false,
})
-- Zig
require("zig-tools").setup()
-- Other
require("toggleterm").setup({
	shade_terminals = false,
	direction = "horizontal",
})
require("harpoon").setup({
	menu = {
		width = 100,
		height = 20,
	}
})
require("gitsigns").setup({
	signs = {
		add          = { text = '+' },
		change       = { text = '~' },
		delete       = { text = '-' },
		topdelete    = { text = '_' },
		changedelete = { text = ':' },
		untracked    = { text = '/' },
	}
})
require("aerial").setup({
	backends = {"treesitter", "lsp", "markdown", "asciidoc", "man"}
})
require("oil").setup({
	default_file_explorer = false,
	view_options = {
		show_hidden = true,
	}
})
require("hex").setup()
require("mini.pairs").setup()
require("nvim-web-devicons").setup()
require("neofusion").setup({
	terminal_colors = true,
	undercurl = true,
	underline = true,
	bold = true,
	italic = {
		strings = true,
		emphasis = true,
		comments = true,
		operators = false,
		folds = true,
	},
	strikethrough = true,
	invert_selection = false,
	invert_signs = false,
	invert_tabline = false,
	invert_intend_guides = false,
	inverse = true,
	palette_overrides = {},
	overrides = {},
	dim_inactive = false,
	transparent_mode = false})

vim.keymap.set("n", "`", "<nop>")
vim.keymap.set("v", "`", "<nop>")
vim.keymap.set("t", "`", "<nop>")
vim.keymap.set("x", "<leader>p", "\"_dP")
vim.keymap.set("n", "<leader>a", "ggVG")
vim.keymap.set("n", "<leader>mm", function()
	require("harpoon.ui").toggle_quick_menu()
end)
vim.keymap.set("n", "<leader>fm", function()
	vim.cmd[[Telescope harpoon marks]]
end)
vim.keymap.set("n", "<leader>mn", function()
	require("harpoon.ui").nav_next()
end)
vim.keymap.set("n", "<leader>mp", function()
	require("harpoon.ui").nav_prev()
end)
vim.keymap.set("n", "<leader>ma", function()
	require("harpoon.mark").add_file()
end)
vim.keymap.set("n", "<leader>m1", function()
	require("harpoon.ui").nav_file(1)
end)
vim.keymap.set("n", "<leader>m2", function()
	require("harpoon.ui").nav_file(2)
end)
vim.keymap.set("n", "<leader>m3", function()
	require("harpoon.ui").nav_file(3)
end)
vim.keymap.set("n", "<leader>m4", function()
	require("harpoon.ui").nav_file(4)
end)
vim.keymap.set("n", "<leader>m5", function()
	require("harpoon.ui").nav_file(5)
end)
vim.keymap.set("n", "<leader>m6", function()
	require("harpoon.ui").nav_file(6)
end)
vim.keymap.set("n", "<leader>m7", function()
	require("harpoon.ui").nav_file(7)
end)
vim.keymap.set("n", "<leader>m8", function()
	require("harpoon.ui").nav_file(8)
end)
vim.keymap.set("n", "<leader>m9", function()
	require("harpoon.ui").nav_file(9)
end)
vim.keymap.set("n", "<C-c><C-l>", function()
	vim.cmd[[wa]] vim.cmd[[make]]
	--vim.cmd[[wa]] vim.cmd[[CMakeBuild]]
	--vim.cmd[[wa]] vim.cmd[[Zig build run]]
end)
vim.keymap.set("n", "<C-c><C-r>", function() vim.cmd[[CMakeRun]] end)
vim.keymap.set("n", "<leader>qq", function() vim.cmd[[copen]] end)
vim.keymap.set("n", "<leader>qx", function() vim.cmd[[cclose]] end)
vim.keymap.set("n", "<leader>ss", function()
	require("telescope.builtin").current_buffer_fuzzy_find()
end)
vim.keymap.set("n", "<leader>sf", function()
	require("telescope.builtin").live_grep()
end)
vim.keymap.set("n", "<leader>sl", function()
	require("telescope.builtin").grep_string({search = vim.fn.input(": ")})
end)
vim.keymap.set("n", "<leader>..", function() vim.cmd[[Oil]] end)
vim.keymap.set("n", "<leader> ", function()
	require("telescope.builtin").commands()
end)
vim.keymap.set("n", "<leader>ff", function()
	require("telescope.builtin").lsp_document_symbols()
end)
vim.keymap.set("n", "<leader>fa", function()
	require("telescope").extensions.aerial.aerial()
end)
vim.keymap.set("n", "<leader>ft", function()
	require("telescope.builtin").treesitter()
end)
vim.keymap.set("n", "<leader>f.", function()
	require("telescope.builtin").find_files()
end)
vim.keymap.set("n", "<leader>fk", function()
	require("telescope.builtin").lsp_workspace_symbols()
end)
vim.keymap.set("n", "<leader>fh", function()
	require("telescope.builtin").help_tags()
end)
vim.keymap.set("n", "<leader>fd", function()
	require("telescope.builtin").diagnostics()
end)
vim.keymap.set("n", "<leader>fj", function()
	require("telescope.builtin").jumplist()
end)
vim.keymap.set("n", "<leader>fr", function()
	require("telescope.builtin").registers()
end)
vim.keymap.set("n", "<leader>fqq", function()
	require("telescope.builtin").quickfix()
end)
vim.keymap.set("n", "<leader>fqh", function()
	require("telescope.builtin").quickfixhistory()
end)
vim.keymap.set("n", "<leader>bb", function()
	require("telescope.builtin").buffers()
end)
vim.keymap.set("n", "<leader>bn", function() vim.cmd[[enew]] end)
vim.keymap.set("n", "<leader>bx", function() vim.cmd[[b#|bd!#]] end)
vim.keymap.set("n", "<leader>tt", function()
	require("telescope-tabs").list_tabs()
end)
vim.keymap.set("n", "<leader>tn", function() vim.cmd[[tab split]] end)
vim.keymap.set("n", "<leader>to", function() vim.cmd[[tab split]] end)
vim.keymap.set("n", "<leader>tx", function() vim.cmd[[tabclose]] end)
vim.keymap.set("n", "<leader>gg", function() vim.cmd[[Git]] end)
vim.keymap.set("n", "<leader>gp", function() vim.cmd[[Git push]] end)
vim.keymap.set("n", "<leader>gd", function() vim.cmd[[Gdiffsplit]] end)
vim.keymap.set("n", "<leader>dv", function() require("telescope").extensions.dap.variables() end)
vim.keymap.set("n", "<leader>df", function() require("telescope").extensions.dap.frames() end)
vim.keymap.set("n", "<leader>dc", function() require("telescope").extensions.dap.commands() end)
vim.keymap.set("n", "<leader>db", function() require("telescope").extensions.dap.list_breakpoints() end)
vim.keymap.set("n", "<leader>dq", function() require("telescope").extensions.dap.configurations() end)
vim.keymap.set("n", "<leader>dr", function() require("dap").repl.toggle() end)
vim.keymap.set("n", "<leader>dl", function() require("dap").run_last() end)
vim.keymap.set("n", "<leader>dd", function() require("dap").continue() end)
vim.keymap.set("n", "<leader>dx", function() vim.cmd[[DapTerminate]] end)
vim.keymap.set("n", "<leader>dw", function() require("dapui").toggle() end)
vim.keymap.set("n", "<A-b>", function() require("dap").toggle_breakpoint() end)
vim.keymap.set("n", "<A-n>", function() require("dap").step_over() end)
vim.keymap.set("n", "<A-j>", function() require("dap").step_into() end)
vim.keymap.set("n", "<A-p>", function() require("dap").step_out() end)
vim.keymap.set("n", "<C-w>1", "<C-w><C-o>")
vim.keymap.set("n", "<C-w>2", "<C-w>v")
vim.keymap.set("n", "<C-w>3", "<C-w>n")
vim.keymap.set("n", "<C-h>", "<C-w>h")
vim.keymap.set("n", "<C-j>", "<C-w>j")
vim.keymap.set("n", "<C-k>", "<C-w>k")
vim.keymap.set("n", "<C-l>", "<C-w>l")
vim.keymap.set("n", "<C-n>", "<cmd>cnext<CR>zz")
vim.keymap.set("n", "<C-p>", "<cmd>cprev<CR>zz")
vim.keymap.set("n", "<leader>k", "<cmd>lnext<CR>zz")
vim.keymap.set("n", "<leader>j", "<cmd>lprev<CR>zz")
vim.keymap.set("n", "<C-Up>", function() vim.cmd[[resize -1]] end)
vim.keymap.set("n", "<C-Down>", function() vim.cmd[[resize +1]] end)
vim.keymap.set("n", "<C-Right>", function() vim.cmd[[vertical resize -1]] end)
vim.keymap.set("n", "<C-Left>", function() vim.cmd[[vertical resize +1]] end)
vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv")
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv")
vim.keymap.set("n", "{", "<cmd>AerialPrev<CR>")
vim.keymap.set("n", "}", "<cmd>AerialNext<CR>")
vim.keymap.set("v", "{", "<cmd>AerialPrev<CR>")
vim.keymap.set("v", "}", "<cmd>AerialNext<CR>")
vim.cmd [[colorscheme neofusion]]
vim.api.nvim_set_hl(0, "Normal", {fg = "none", bg = "none"})
vim.api.nvim_set_hl(0, "NormalNC", {fg = "none", bg = "none"})
vim.api.nvim_set_hl(0, "NormalFloat", {fg = "none", bg = "none"})
vim.api.nvim_set_hl(0, "FloatBorder", {fg = "none", bg = "none"})
vim.api.nvim_set_hl(0, "EndOfBuffer", {fg = "none", bg = "none"})
--vim.api.nvim_set_hl(0, "ColorColumn", {fg = "none", bg = "none"})
vim.api.nvim_set_hl(0, "LineNr", {fg = "none", bg = "none"})
vim.api.nvim_set_hl(0, "SignColumn", {fg = "none", bg = "none"})
