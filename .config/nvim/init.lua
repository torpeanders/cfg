-- Bootstrap lazy.nvim (plugin manager)
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({"git", "clone", "--filter=blob:none", "https://github.com/folke/lazy.nvim.git", "--branch=stable", lazypath})
end
vim.opt.rtp:prepend(lazypath)

-- Plugin Setup
require("lazy").setup({
  "nvim-lua/plenary.nvim",                    -- Dependency for other plugins
  "nvim-telescope/telescope.nvim",            -- Fuzzy finder replacement
  "lewis6991/gitsigns.nvim",                  -- Git integration
  "tpope/vim-fugitive",                       -- Git wrapper
  "nvim-treesitter/nvim-treesitter",          -- Better syntax highlighting
  "neovim/nvim-lspconfig",                    -- Built-in LSP support
  "hrsh7th/nvim-cmp",                         -- Autocompletion
  "hrsh7th/cmp-nvim-lsp",                     -- LSP completion
  "L3MON4D3/LuaSnip",                         -- Snippets
  "itchyny/lightline.vim",                    -- Status line
  "folke/flash.nvim",
  "folke/which-key.nvim",                     -- Keybinding helper
  { "catppuccin/nvim", name = "catppuccin" }, -- Dark theme with good C/C++ contrast
})

-- General Settings
vim.opt.breakindent = true
vim.opt.autoindent = true
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true
vim.opt.background = "dark"
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.hlsearch = true
vim.opt.incsearch = true
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.termguicolors = true
vim.g.mapleader = ","

-- Keybindings
vim.keymap.set("n", "<leader>.", ":lcd %:p:h<CR>", { noremap = true, silent = true })

-- Window Navigation
vim.keymap.set("n", "<leader>h", "<C-w>h", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>l", "<C-w>l", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>j", "<C-w>j", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>k", "<C-w>k", { noremap = true, silent = true })

-- LSP Configuration
local lspconfig = require("lspconfig")
lspconfig.ts_ls.setup{}
lspconfig.pyright.setup{}

-- Autocompletion Setup
local cmp = require("cmp")
cmp.setup({
  mapping = cmp.mapping.preset.insert({
    ["<Tab>"] = cmp.mapping.select_next_item(),
    ["<S-Tab>"] = cmp.mapping.select_prev_item(),
    ["<CR>"] = cmp.mapping.confirm({ select = true }),
  }),
  sources = cmp.config.sources({
    { name = "nvim_lsp" },
    { name = "luasnip" },
  })
})

-- Telescope Setup
require("telescope").setup{}
vim.keymap.set("n", "<leader>ff", ":Telescope find_files<CR>", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>fg", ":Telescope live_grep<CR>", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>fb", ":Telescope buffers<CR>", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>fh", ":Telescope help_tags<CR>", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>fl", ":Telescope current_buffer_fuzzy_find<CR>", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>fr", function() require("telescope.builtin").oldfiles() end, { noremap = true, silent = true, desc = "Open recent files" })

-- Git Signs
require("gitsigns").setup({
  on_attach = function(bufnr)
    local gs = package.loaded.gitsigns

    -- Navigation
    vim.keymap.set("n", "]c", function() gs.next_hunk() end, { buffer = bufnr, desc = "Next Hunk" })
    vim.keymap.set("n", "[c", function() gs.prev_hunk() end, { buffer = bufnr, desc = "Prev Hunk" })

    -- Actions
    vim.keymap.set("n", "<leader>hs", function() gs.stage_hunk() end, { buffer = bufnr, desc = "Stage Hunk" })
    vim.keymap.set("n", "<leader>hu", function() gs.undo_stage_hunk() end, { buffer = bufnr, desc = "Undo Stage Hunk" })
    vim.keymap.set("n", "<leader>hr", function() gs.reset_hunk() end, { buffer = bufnr, desc = "Reset Hunk" })
    vim.keymap.set("n", "<leader>hR", function() gs.reset_buffer() end, { buffer = bufnr, desc = "Reset Buffer" })
    vim.keymap.set("n", "<leader>hp", function() gs.preview_hunk() end, { buffer = bufnr, desc = "Preview Hunk" })
    vim.keymap.set("n", "<leader>hb", function() gs.blame_line() end, { buffer = bufnr, desc = "Blame Line" })
  end
})

-- Flash
require("flash").setup()
vim.keymap.set("n", "s", function() require("flash").jump() end, { desc = "Jump anywhere" })
vim.keymap.set("n", "<leader>j", function() require("flash").jump({ search = { forward = true, wrap = false } }) end, { desc = "Jump forward" })
vim.keymap.set("n", "<leader>k", function() require("flash").jump({ search = { forward = false, wrap = false } }) end, { desc = "Jump backward" })

-- Which Key
require("which-key").setup({})

-- Color Theme
vim.cmd.colorscheme("catppuccin-mocha") -- Use the "mocha" variant for a darker look

