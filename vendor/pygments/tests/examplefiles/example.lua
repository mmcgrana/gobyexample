--[[
	Auctioneer Advanced
	Version: <%version%> (<%codename%>)
	Revision: $Id: CoreMain.lua 2233 2007-09-25 03:57:33Z norganna $
	URL: http://auctioneeraddon.com/

	This is an addon for World of Warcraft that adds statistical history to the auction data that is collected
	when the auction is scanned, so that you can easily determine what price
	you will be able to sell an item for at auction or at a vendor whenever you
	mouse-over an item in the game

	License:
		This program is free software; you can redistribute it and/or
		modify it under the terms of the GNU General Public License
		as published by the Free Software Foundation; either version 2
		of the License, or (at your option) any later version.

		This program is distributed in the hope that it will be useful,
		but WITHOUT ANY WARRANTY; without even the implied warranty of
		MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
		GNU General Public License for more details.

		You should have received a copy of the GNU General Public License
		along with this program(see GPL.txt); if not, write to the Free Software
		Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

	Note:
		This AddOn's source code is specifically designed to work with
		World of Warcraft's interpreted AddOn system.
		You have an implicit licence to use this AddOn with these facilities
		since that is its designated purpose as per:
		http://www.fsf.org/licensing/licenses/gpl-faq.html#InterpreterIncompat
]]


--[[
	See CoreAPI.lua for a description of the modules API
]]

if (not AucAdvanced) then AucAdvanced = {} end
if (not AucAdvancedData) then AucAdvancedData = {} end
if (not AucAdvancedLocal) then AucAdvancedLocal = {} end
if (not AucAdvancedConfig) then AucAdvancedConfig = {} end

AucAdvanced.Version="<%version%>";
if (AucAdvanced.Version == "<".."%version%>") then
	AucAdvanced.Version = "5.0.DEV";
end

local private = {}

-- For our modular stats system, each stats engine should add their
-- subclass to AucAdvanced.Modules.<type>.<name> and store their data into their own
-- data table in AucAdvancedData.Stats.<type><name>
if (not AucAdvanced.Modules) then AucAdvanced.Modules = {Stat={},Util={},Filter={}} end
if (not AucAdvancedData.Stats) then AucAdvancedData.Stats = {} end
if (not AucAdvancedLocal.Stats) then AucAdvancedLocal.Stats = {} end

function private.TooltipHook(vars, ret, frame, name, hyperlink, quality, quantity, cost, additional)
	if EnhTooltip.LinkType(hyperlink) ~= "item" then
		return -- Auctioneer hooks into item tooltips only
	end

	-- Check to see if we need to force load scandata
	local getter = AucAdvanced.Settings.GetSetting
	if (getter("scandata.tooltip.display") and getter("scandata.force")) then
		AucAdvanced.Scan.GetImage()
	end

	for system, systemMods in pairs(AucAdvanced.Modules) do
		for engine, engineLib in pairs(systemMods) do
			if (engineLib.Processor) then engineLib.Processor("tooltip", frame, name, hyperlink, quality, quantity, cost, additional) end
		end
	end
end

function private.HookAH()
	hooksecurefunc("AuctionFrameBrowse_Update", AucAdvanced.API.ListUpdate)
	for system, systemMods in pairs(AucAdvanced.Modules) do
		for engine, engineLib in pairs(systemMods) do
			if (engineLib.Processor) then
				engineLib.Processor("auctionui")
			end
		end
	end
end

function private.OnLoad(addon)
	addon = addon:lower()

	-- Check if the actual addon itself is loading
	if (addon == "auc-advanced") then
		Stubby.RegisterAddOnHook("Blizzard_AuctionUi", "Auc-Advanced", private.HookAH)
		Stubby.RegisterFunctionHook("EnhTooltip.AddTooltip", 600, private.TooltipHook)
		for pos, module in ipairs(AucAdvanced.EmbeddedModules) do
			-- These embedded modules have also just been loaded
			private.OnLoad(module)
		end
	end

	-- Notify the actual module if it exists
	local auc, sys, eng = strsplit("-", addon)
	if (auc == "auc" and sys and eng) then
		for system, systemMods in pairs(AucAdvanced.Modules) do
			if (sys == system:lower()) then
				for engine, engineLib in pairs(systemMods) do
					if (eng == engine:lower() and engineLib.OnLoad) then
						engineLib.OnLoad(addon)
					end
				end
			end
		end
	end

	-- Check all modules' load triggers and pass event to processors
	for system, systemMods in pairs(AucAdvanced.Modules) do
		for engine, engineLib in pairs(systemMods) do
			if (engineLib.LoadTriggers and engineLib.LoadTriggers[addon]) then
				if (engineLib.OnLoad) then
					engineLib.OnLoad(addon)
				end
			end
			if (engineLib.Processor and auc == "auc" and sys and eng) then
				engineLib.Processor("load", addon)
			end
		end
	end
end

function private.OnUnload()
	for system, systemMods in pairs(AucAdvanced.Modules) do
		for engine, engineLib in pairs(systemMods) do
			if (engineLib.OnUnload) then
				engineLib.OnUnload()
			end
		end
	end
end

private.Schedule = {}
function private.OnEvent(...)
	local event, arg = select(2, ...)
	if (event == "ADDON_LOADED") then
		local addon = string.lower(arg)
		if (addon:sub(1,4) == "auc-") then
			private.OnLoad(addon)
		end
	elseif (event == "AUCTION_HOUSE_SHOW") then
		-- Do Nothing for now
	elseif (event == "AUCTION_HOUSE_CLOSED") then
		AucAdvanced.Scan.Interrupt()
	elseif (event == "PLAYER_LOGOUT") then
		AucAdvanced.Scan.Commit(true)
		private.OnUnload()
	elseif event == "UNIT_INVENTORY_CHANGED"
	or event == "ITEM_LOCK_CHANGED"
	or event == "CURSOR_UPDATE"
	or event == "BAG_UPDATE"
	then
		private.Schedule["inventory"] = GetTime() + 0.15
	end
end

function private.OnUpdate(...)
	if event == "inventory" then
		AucAdvanced.Post.AlertBagsChanged()
	end

	local now = GetTime()
	for event, time in pairs(private.Schedule) do
		if time > now then
			for system, systemMods in pairs(AucAdvanced.Modules) do
				for engine, engineLib in pairs(systemMods) do
					if engineLib.Processor then
						engineLib.Processor(event, time)
					end
				end
			end
		end
		private.Schedule[event] = nil
	end
end

private.Frame = CreateFrame("Frame")
private.Frame:RegisterEvent("ADDON_LOADED")
private.Frame:RegisterEvent("AUCTION_HOUSE_SHOW")
private.Frame:RegisterEvent("AUCTION_HOUSE_CLOSED")
private.Frame:RegisterEvent("UNIT_INVENTORY_CHANGED")
private.Frame:RegisterEvent("ITEM_LOCK_CHANGED")
private.Frame:RegisterEvent("CURSOR_UPDATE")
private.Frame:RegisterEvent("BAG_UPDATE")
private.Frame:RegisterEvent("PLAYER_LOGOUT")
private.Frame:SetScript("OnEvent", private.OnEvent)
private.Frame:SetScript("OnUpdate", private.OnUpdate)

-- Auctioneer's debug functions
AucAdvanced.Debug = {}
local addonName = "Auctioneer" -- the addon's name as it will be displayed in
                               -- the debug messages
-------------------------------------------------------------------------------
-- Prints the specified message to nLog.
--
-- syntax:
--    errorCode, message = debugPrint([message][, category][, title][, errorCode][, level])
--
-- parameters:
--    message   - (string) the error message
--                nil, no error message specified
--    category  - (string) the category of the debug message
--                nil, no category specified
--    title     - (string) the title for the debug message
--                nil, no title specified
--    errorCode - (number) the error code
--                nil, no error code specified
--    level     - (string) nLog message level
--                         Any nLog.levels string is valid.
--                nil, no level specified
--
-- returns:
--    errorCode - (number) errorCode, if one is specified
--                nil, otherwise
--    message   - (string) message, if one is specified
--                nil, otherwise
-------------------------------------------------------------------------------
function AucAdvanced.Debug.DebugPrint(message, category, title, errorCode, level)
	return DebugLib.DebugPrint(addonName, message, category, title, errorCode, level)
end

-------------------------------------------------------------------------------
-- Used to make sure that conditions are met within functions.
-- If test is false, the error message will be written to nLog and the user's
-- default chat channel.
--
-- syntax:
--    assertion = assert(test, message)
--
-- parameters:
--    test    - (any)     false/nil, if the assertion failed
--                        anything else, otherwise
--    message - (string)  the message which will be output to the user
--
-- returns:
--    assertion - (boolean) true, if the test passed
--                          false, otherwise
-------------------------------------------------------------------------------
function AucAdvanced.Debug.Assert(test, message)
	return DebugLib.Assert(addonName, test, message)
end


