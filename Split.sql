/********************************************************************************************************
Description:    Splits a string using the specified delimiter. Returns a table where each row contains
                the position and value of the item.
*********************************************************************************************************/
CREATE FUNCTION [dbo].[Split] (
     @Delimiter     NVARCHAR(32) = ','
    ,@Text          NVARCHAR(max))
RETURNS @Results TABLE (
     [Position]     INT IDENTITY(1,1) NOT NULL
    ,[Value]        NVARCHAR(MAX)
)
AS
BEGIN

-- Validate parameters
SET @Delimiter=ISNULL(@Delimiter,',')

-- Escape XML characters
SET @Delimiter = REPLACE(@Delimiter, '&', '&amp;')
SET @Delimiter = REPLACE(@Delimiter, '"', '&quot;')
SET @Delimiter = REPLACE(@Delimiter, '''', '&apos;')
SET @Delimiter = REPLACE(@Delimiter, '<', '&lt;')
SET @Delimiter = REPLACE(@Delimiter, '>', '&gt;')
SET @Text = REPLACE(@Text, '&', '&amp;')
SET @Text = REPLACE(@Text, '"', '&quot;')
SET @Text = REPLACE(@Text, '''', '&apos;')
SET @Text = REPLACE(@Text, '<', '&lt;')
SET @Text = REPLACE(@Text, '>', '&gt;')

DECLARE @xml XML
SET @XML = N'<root><r>' + REPLACE(@Text, @Delimiter, '</r><r>') + '</r></root>'

INSERT INTO @Results([value])
SELECT r.value('.','NVARCHAR(MAX)') as Item
FROM @xml.nodes('//root/r') AS RECORDS(r)

RETURN
END

-- Testing code
 --DROP FUNCTION [dbo].[Split]
 --SELECT * FROM [dbo].[Split](NULL, NULL)
 --SELECT * FROM [dbo].[Split](NULL, 'First,Second,Third,Fourth')
 --SELECT * FROM [dbo].[Split](default, 'First,Second,Third,Fourth')
 --SELECT * FROM [dbo].[Split](default, 'First,,Third,,Fifth')
 --SELECT * FROM [dbo].[Split]('|', 'First|Second|Third|Fourth')
 --SELECT * FROM [dbo].[Split]('<', 'First<Second<Third<Fourth')
 --SELECT * FROM [dbo].[Split]('>', 'First>Second>Third>Fourth')
 --SELECT * FROM [dbo].[Split]('"', 'First"Second"Third"Fourth')
 --SELECT * FROM [dbo].[Split]('''', 'First''Second''Third''Fourth')
 --SELECT * FROM [dbo].[Split]('&', 'First&Second&Third&Fourth')
 --SELECT * FROM [dbo].[Split]('&', 'First&"Second Column"&Third "3rd"&Fourth');


