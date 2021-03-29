/********************************************************************************************************
Description:    Utility procedure to PRINT without any limitation on text size.
*********************************************************************************************************/
CREATE PROCEDURE [dbo].[PrintMax] (
     @Text nvarchar(max) = NULL
    ,@MaxPrintLength int = 4000
)
AS
BEGIN
    SET NOCOUNT ON
 
    -- CONSTANTS
    DECLARE @LF         char(1)
    DECLARE @CR         char(1)
    DECLARE @CRLF       char(2)
    DECLARE @LINE_BREAK char(3)

    SET @LF         = CHAR(10)
    SET @CR         = CHAR(13)
    SET @CRLF       = CHAR(13) + CHAR(10)
    SET @LINE_BREAK = '%' + @LF + '%'

    -- Working Values
    DECLARE @WorkingLength    bigint
    DECLARE @WorkingString    nvarchar(max)
    DECLARE @SubString        nvarchar(max)
    DECLARE @SubStringLength  bigint
 
    -- Validate/correct inputs
    SET @MaxPrintLength = COALESCE(NULLIF(@MaxPrintLength, 0), 4000)
 
    IF @MaxPrintLength > 4000
        BEGIN
            RAISERROR('The @MaxPrintLength value of %d is greater than the maximum length supported by PRINT for nvarchar strings (4000)', 17, 1, @MaxPrintLength);
            RETURN(60000);
        END
 
    -- Working variables
    DECLARE @InputLength bigint = LEN(@Text)
 
    IF @InputLength = 0
        GOTO OnComplete
 
    -- Our input string may contain either carriage returns, line feeds or both
    -- to separate printing lines so we need to standardise on one of these (LF)
    SET @WorkingString = REPLACE(REPLACE(@Text, @CRLF, @LF), @CR, @LF);
 
    -- If there are line feeds we use those to break down the text
    -- into individual printed lines, otherwise we print it in
    -- bite-size chunks suitable for consumption by PRINT
    IF PATINDEX(@LINE_BREAK, @Text) > 0
    BEGIN
 
        -- Add a line feed on the end so the final iteration works as expected
        SET @WorkingString  = @WorkingString + @LF;
        SET @WorkingLength  = LEN(@WorkingString);
 
        DECLARE @LineFeedPos    bigint    = 0
 
        WHILE @WorkingLength > 0
            BEGIN
                -- Get the position of the next line feed
                SET @LineFeedPos = PATINDEX(@LINE_BREAK, @WorkingString);
 
                IF @LineFeedPos > 0
                    BEGIN
                        SET @SubString        = SUBSTRING(@WorkingString, 1, @LineFeedPos - 1);
                        SET @SubStringLength  = LEN(@SubString);
 
                        -- If this string is too long for a single PRINT, we pass it back
                        -- to PrintString which will process the string in suitably sized chunks
                        IF LEN(@SubString) > @MaxPrintLength
                            EXEC [PrintMax] @Text = @SubString
                        ELSE
                            PRINT @SubString;
 
                        -- Remove the text we've just processed
                        SET @WorkingLength    = @WorkingLength - @LineFeedPos;
                        SET @WorkingString    = SUBSTRING(@WorkingString, @LineFeedPos + 1, @WorkingLength);
                    END
            END
 
    END
    ELSE
    BEGIN
        -- If there are no line feeds we may have to break it down
        -- into smaller bit size chunks suitable for PRINT
        IF @InputLength > @MaxPrintLength
            BEGIN
                SET @WorkingString    = @Text;
                SET @WorkingLength    = LEN(@WorkingString);
                SET @SubStringLength  = @MaxPrintLength;
 
                WHILE @WorkingLength > 0
                    BEGIN
                        SET @SubString        = SUBSTRING(@WorkingString, 1, @SubStringLength);
                        SET @SubStringLength  = LEN(@SubString)
 
                        -- If we still have text to process, set working values
                        IF (@WorkingLength - @SubStringLength + 1) > 0
                            BEGIN
                                PRINT @SubString;
                                -- Remove the text we've just processed
                                SET @WorkingString    = SUBSTRING(@WorkingString, @SubStringLength + 1, @WorkingLength);
                                SET @WorkingLength    = LEN(@WorkingString);
                            END
                    END
            END
        ELSE
            PRINT @Text;
 
    END
 
OnComplete:
    SET NOCOUNT OFF
    RETURN
END
GO

GRANT EXECUTE ON [dbo].[PrintMax] TO [public]
GO
